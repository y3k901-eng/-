(vl-load-com)

;; ============================================================
;; ★ 설정
;; ============================================================

(setq CAPREC_REGKEY "HKEY_CURRENT_USER\\Software\\CAPREC_SETTINGS")
(setq *CAPREC_LAYER* "판넬그룹핑")

;; ============================================================
;; ★ 영구메모리(레지스트리) 유틸
;; ============================================================

(defun D_REG_GET_STR (name / v) (vl-registry-read CAPREC_REGKEY name))
(defun D_REG_SET_STR (name val) (vl-registry-write CAPREC_REGKEY name val))

(defun D_REG_GET_REAL (name def / v)
  (setq v (D_REG_GET_STR name))
  (if v (atof v) def)
)

(defun D_REG_SET_REAL (name val)
  (D_REG_SET_STR name (rtos val 2 6))
)

(defun D_GETREAL_PERSIST (msg regKey def / cur val)
  (setq cur (D_REG_GET_REAL regKey def))
  (setq val (getreal (strcat "\n" msg " <" (rtos cur 2 2) ">: ")))
  (if (null val) (setq val cur))
  (D_REG_SET_REAL regKey val)
  val
)

;; ============================================================
;; 0) Small utils + Layer/Color
;; ============================================================

(defun D_ENAMEP (x) (= (type x) 'ENAME))

(defun D_SPLIT (s delim / out p ld)
  (setq out '())
  (setq s (if s s ""))
  (setq ld (strlen delim))
  (if (<= ld 0)
    (list s)
    (progn
      (while (setq p (vl-string-search delim s))
        (setq out (cons (substr s 1 p) out))
        (setq s (substr s (+ p ld 1)))
      )
      (reverse (cons s out))
    )
  )
)

(defun D_PAD4 (n / s)
  (setq s (itoa n))
  (cond
    ((= (strlen s) 1) (strcat "000" s))
    ((= (strlen s) 2) (strcat "00" s))
    ((= (strlen s) 3) (strcat "0" s))
    (T s)
  )
)

(defun D_GNAME (idx) (strcat "PP-" (D_PAD4 idx)))
(defun D_TRIM (s) (vl-string-trim " " (if s s "")))

;; ----- Layer ensure -----
(defun D_LAYER_EXISTS_P (name) (if (tblsearch "LAYER" name) T nil))

(defun D_LAYER_ENSURE (name / e)
  (if (not (D_LAYER_EXISTS_P name))
    (progn
      (setq e
        (entmakex
          (list
            (cons 0 "LAYER")
            (cons 2 name)
            (cons 70 0)
            (cons 62 7)
            (cons 6 "CONTINUOUS")
          )
        )
      )
      (if (not (D_ENAMEP e))
        (princ (strcat "\n[경고] 레이어 생성 실패: " name))
      )
    )
  )
  name
)

(defun D_SET_CUR_LAYER (name / old)
  (setq old (getvar "CLAYER"))
  (if (/= old name) (setvar "CLAYER" name))
  old
)

;; ============================================================
;; ✅ 그룹 색상 팔레트(48개): 요청 고정
;;  - 10,20,...,240 (24개)
;;  - 11,21,...,241 (24개)
;; ============================================================

(defun D_BUILD_COLOR_PALETTE_48 (/ out i)
  (setq out '())
  (setq i 10)
  (while (<= i 240)
    (setq out (append out (list i)))
    (setq i (+ i 10))
  )
  (setq i 11)
  (while (<= i 241)
    (setq out (append out (list i)))
    (setq i (+ i 10))
  )
  out
)

(setq *CAPREC_COLOR_PALETTE* (D_BUILD_COLOR_PALETTE_48))

(defun D_COLOR_FOR_IDX (idx / n)
  (setq n (length *CAPREC_COLOR_PALETTE*))
  (if (<= n 0)
    10
    (nth (rem (max 0 (1- idx)) n) *CAPREC_COLOR_PALETTE*)
  )
)

(defun D_ENT_SET_COLOR (e aci / ed)
  (if (D_ENAMEP e)
    (progn
      (setq ed (entget e))
      (if (assoc 62 ed)
        (setq ed (subst (cons 62 aci) (assoc 62 ed) ed))
        (setq ed (append ed (list (cons 62 aci))))
      )
      (entmod ed)
      (entupd e)
    )
  )
)

;; ============================================================
;; ✅ 파싱: ';' 만 구분자로 사용
;; ============================================================

(defun D_PARSE_TOKENS (s / ss)
  (setq ss (D_TRIM s))
  (if (= ss "")
    nil
    (if (vl-string-search ";" ss)
      (D_SPLIT ss ";")
      nil
    )
  )
)

;; ============================================================
;; 1) TEXT / parsing
;; [용량;수용율;전원종류;전압;상;차단기사이즈;대공정;소공정;차단기;설비명;부하명]
;; ============================================================

(defun D_GET_TEXT (e / ed s)
  (if (not (D_ENAMEP e))
    nil
    (progn
      (setq ed (entget e))
      (cond
        ((= (cdr (assoc 0 ed)) "TEXT") (cdr (assoc 1 ed)))
        ((= (cdr (assoc 0 ed)) "MTEXT")
         (setq s (cdr (assoc 1 ed)))
         (foreach p ed (if (= (car p) 3) (setq s (strcat s (cdr p)))))
         s
        )
        (T nil)
      )
    )
  )
)

(defun D_PARSE_CAP (s / parts cap)
  (setq parts (D_PARSE_TOKENS s))
  (if (or (null parts) (< (length parts) 2))
    nil
    (progn
      (setq cap (atof (car parts)))
      (if (> cap 0.0) cap nil)
    )
  )
)

(defun D_PARSE_BRK_SIZE (s / parts v)
  (setq parts (D_PARSE_TOKENS s))
  (if (or (null parts) (< (length parts) 6))
    nil
    (progn
      (setq v (atof (nth 5 parts)))
      (if (> v 0.0) v nil)
    )
  )
)

(defun D_PARSE_BASE (s / parts b)
  (setq parts (D_PARSE_TOKENS s))
  (if (and parts (>= (length parts) 10))
    (progn (setq b (nth 9 parts)) (D_TRIM b))
    ""
  )
)

;; ============================================================
;; 2) BBOX + circle test
;; ============================================================

(defun D_GET_BBOX (ename / obj mn mx)
  (if (not (D_ENAMEP ename))
    nil
    (progn
      (setq obj (vlax-ename->vla-object ename))
      (if (or (null obj) (/= (type obj) 'VLA-OBJECT))
        nil
        (if (vl-catch-all-error-p
              (vl-catch-all-apply '(lambda () (vla-getboundingbox obj 'mn 'mx)))
            )
          nil
          (list (vlax-safearray->list mn)
                (vlax-safearray->list mx))
        )
      )
    )
  )
)

(defun D_BBOX_UNION (bb1 bb2 / a1 a2 b1 b2)
  (setq a1 (car bb1) a2 (cadr bb1))
  (setq b1 (car bb2) b2 (cadr bb2))
  (list
    (list (min (car a1) (car b1)) (min (cadr a1) (cadr b1)) 0.0)
    (list (max (car a2) (car b2)) (max (cadr a2) (cadr b2)) 0.0)
  )
)

(defun D_BBOX_AREA2D (bb / mn mx w h)
  (setq mn (car bb) mx (cadr bb))
  (setq w (- (car mx) (car mn)))
  (setq h (- (cadr mx) (cadr mn)))
  (if (or (<= w 0.0) (<= h 0.0))
    0.0
    (* w h)
  )
)

(defun D_BBOX_INTERSECT (bb1 bb2 / a1 a2 b1 b2 ix1 iy1 ix2 iy2)
  (setq a1 (car bb1) a2 (cadr bb1))
  (setq b1 (car bb2) b2 (cadr bb2))
  (setq ix1 (max (car a1) (car b1)))
  (setq iy1 (max (cadr a1) (cadr b1)))
  (setq ix2 (min (car a2) (car b2)))
  (setq iy2 (min (cadr a2) (cadr b2)))
  (if (or (<= ix2 ix1) (<= iy2 iy1))
    nil
    (list (list ix1 iy1 0.0) (list ix2 iy2 0.0))
  )
)

(defun D_BBOX_OVERLAP_AREA (bb1 bb2 / ibb)
  (setq ibb (D_BBOX_INTERSECT bb1 bb2))
  (if ibb (D_BBOX_AREA2D ibb) 0.0)
)

(defun D_GROUPS_OVERLAP_COST (bb groups / s g gbb)
  (setq s 0.0)
  (foreach g groups
    (setq gbb (nth 2 g))
    (if gbb (setq s (+ s (D_BBOX_OVERLAP_AREA bb gbb))))
  )
  s
)

(defun D_DIST2D2 (p q / dx dy)
  (setq dx (- (car p) (car q)))
  (setq dy (- (cadr p) (cadr q)))
  (+ (* dx dx) (* dy dy))
)

(defun D_BBOX_DMAX2 (bb / mn mx cx cy c p1 p2 p3 p4 dmax2)
  (setq mn (car bb) mx (cadr bb))
  (setq cx (/ (+ (car mn) (car mx)) 2.0))
  (setq cy (/ (+ (cadr mn) (cadr mx)) 2.0))
  (setq c (list cx cy))
  (setq p1 (list (car mn) (cadr mn)))
  (setq p2 (list (car mx) (cadr mn)))
  (setq p3 (list (car mx) (cadr mx)))
  (setq p4 (list (car mn) (cadr mx)))
  (setq dmax2 (max (D_DIST2D2 c p1) (D_DIST2D2 c p2) (D_DIST2D2 c p3) (D_DIST2D2 c p4)))
  dmax2
)

(defun D_BBOX_FIT_CIRCLE2 (bb R2) (<= (D_BBOX_DMAX2 bb) R2))

(defun D_UNION_BBOX_ALL (items / bb)
  (setq bb nil)
  (foreach it items
    (setq bb (if bb (D_BBOX_UNION bb (nth 2 it)) (nth 2 it)))
  )
  bb
)

(defun D_BBOX_CENTER2D (bb / mn mx)
  (setq mn (car bb) mx (cadr bb))
  (list (/ (+ (car mn) (car mx)) 2.0)
        (/ (+ (cadr mn) (cadr mx)) 2.0))
)

(defun D_ITEM_CENTER (it) (D_BBOX_CENTER2D (nth 2 it)))

;; ============================================================
;; 3) Group structure
;; ============================================================

(defun D_GROUP_INIT_EMPTY () (list 0.0 0 nil '() "" 0.0))

(defun D_GROUP_CAN_ADD_ITEM (g it maxsum maxBrkSum R2 / newSum newBB newBrk)
  (setq newSum (+ (nth 0 g) (cadr it)))
  (setq newBB  (if (nth 2 g) (D_BBOX_UNION (nth 2 g) (nth 2 it)) (nth 2 it)))
  (setq newBrk (+ (nth 5 g) (nth 4 it)))
  (and (<= newSum maxsum)
       (<= newBrk maxBrkSum)
       (D_BBOX_FIT_CIRCLE2 newBB R2))
)

(defun D_GROUP_ADD_ITEM (g it)
  (list
    (+ (nth 0 g) (cadr it))
    (1+ (nth 1 g))
    (if (nth 2 g) (D_BBOX_UNION (nth 2 g) (nth 2 it)) (nth 2 it))
    (append (nth 3 g) (list it))
    (nth 4 g)
    (+ (nth 5 g) (nth 4 it))
  )
)

(defun D_GROUP_LABEL (g / items)
  (setq items (nth 3 g))
  (if items
    (list (nth 0 g) (nth 1 g) (nth 2 g) (nth 3 g) (nth 3 (car items)) (nth 5 g))
    (list (nth 0 g) (nth 1 g) (nth 2 g) (nth 3 g) "" (nth 5 g))
  )
)

(defun D_GROUP_CENTER (g / bb)
  (setq bb (nth 2 g))
  (if bb (D_BBOX_CENTER2D bb) (list 0.0 0.0))
)

;; ============================================================
;; 4) base blocks
;; ============================================================

(defun D_BASEBLOCKS (items / blocks it b pair)
  (setq blocks '())
  (foreach it items
    (setq b (nth 3 it))
    (if (setq pair (assoc b blocks))
      (setq blocks (subst (cons b (append (cdr pair) (list it))) pair blocks))
      (setq blocks (append blocks (list (cons b (list it)))))
    )
  )
  blocks
)

(defun D_PAIR_COUNT (pair) (length (cdr pair)))

(defun D_BLOCK_SUMCAP (blockItems / s it)
  (setq s 0.0)
  (foreach it blockItems (setq s (+ s (cadr it))))
  s
)

(defun D_BLOCK_SUMBRK (blockItems / s it)
  (setq s 0.0)
  (foreach it blockItems (setq s (+ s (nth 4 it))))
  s
)

;; ============================================================
;; ✅ BASE(설비코드) "가능하면 유지" + "한도 초과 시 분리 허용" 블록 생성
;; - BASE별로 묶되, 해당 BASE 블록의 (SUM 또는 BRK SUM)이 한도를 초과하면
;;   -> 그 BASE는 아이템 단위로 분해하여 여러 그룹에 분산 가능하게 함
;; ============================================================

(defun D_BASEBLOCKS_FLEX (items maxsum maxBrkSum / blocks it b pair tmp bi sCap sBrk k idx)
  (setq tmp '())

  ;; 1) BASE별로 모으기
  (foreach it items
    (setq b (nth 3 it)) ;; base
    (if (setq pair (assoc b tmp))
      (setq tmp (subst (cons b (append (cdr pair) (list it))) pair tmp))
      (setq tmp (append tmp (list (cons b (list it)))))
    )
  )

  ;; 2) 한도 초과 BASE는 분해(아이템 단위 블록으로)
  (setq blocks '())
  (foreach pair tmp
    (setq b  (car pair))
    (setq bi (cdr pair))
    (setq sCap (D_BLOCK_SUMCAP bi))
    (setq sBrk (D_BLOCK_SUMBRK bi))

    (if (and (<= sCap maxsum) (<= sBrk maxBrkSum))
      ;; ✅ 한도 내면 BASE 통째로 유지
      (setq blocks (append blocks (list (cons b bi))))
      ;; ✅ 한도 초과면 분리 허용: 아이템 단위 블록으로 쪼갬
      (progn
        (setq idx 1)
        (foreach it bi
          (setq k (strcat b "|SPLIT|" (D_PAD4 idx)))
          (setq blocks (append blocks (list (cons k (list it)))))
          (setq idx (1+ idx))
        )
      )
    )
  )

  blocks
)

(defun D_BLOCK_BBOX (blockItems) (D_UNION_BBOX_ALL blockItems))

(defun D_GROUP_CAN_ADD_BLOCK (g blockItems maxsum maxBrkSum R2 / addCap addBrk newSum newBB)
  (setq addCap (D_BLOCK_SUMCAP blockItems))
  (setq addBrk (D_BLOCK_SUMBRK blockItems))
  (setq newSum (+ (nth 0 g) addCap))
  (setq newBB  (if (nth 2 g) (D_BBOX_UNION (nth 2 g) (D_BLOCK_BBOX blockItems)) (D_BLOCK_BBOX blockItems)))
  (and (<= newSum maxsum)
       (<= (+ (nth 5 g) addBrk) maxBrkSum)
       (D_BBOX_FIT_CIRCLE2 newBB R2))
)

(defun D_GROUP_ADD_BLOCK (g blockItems / it)
  (foreach it blockItems (setq g (D_GROUP_ADD_ITEM g it)))
  g
)

(defun D_REMOVE_PAIR_ONCE (lst target / out removed x)
  (setq out '() removed nil)
  (foreach x lst
    (if (and (null removed) (equal x target))
      (setq removed T)
      (setq out (append out (list x)))
    )
  )
  out
)

;; ============================================================
;; 5) base 분할 억제 + 근접 확장
;; ============================================================

(defun D_PICK_SEED_BLOCK_FIT_FIRST (blocks maxsum maxBrkSum R2 fixedGroups / g0 best bestN bestOv p n pbb pov)
  (setq g0 (D_GROUP_INIT_EMPTY))
  (setq best nil)
  (setq bestN -1)
  (setq bestOv nil)
  (foreach p blocks
    (setq n (D_PAIR_COUNT p))
    (if (D_GROUP_CAN_ADD_BLOCK g0 (cdr p) maxsum maxBrkSum R2)
      (progn
        (setq pbb (D_BLOCK_BBOX (cdr p)))
        (setq pov (D_GROUPS_OVERLAP_COST pbb fixedGroups))
        (if (or (null best)
                (> n bestN)
                (and (= n bestN) (< pov bestOv))
                (and (= n bestN) (equal pov bestOv 1e-9)))
          (progn
            (setq bestN n)
            (setq bestOv pov)
            (setq best p)
          )
        )
      )
    )
  )
  best
)

(defun D_PICK_LARGEST_BLOCK (blocks / best bestN p n)
  (setq best nil bestN -1)
  (foreach p blocks
    (setq n (D_PAIR_COUNT p))
    (if (> n bestN)
      (progn (setq bestN n) (setq best p))
    )
  )
  best
)

(defun D_BUILD_CAND_ITEM_LIST (blocks g fixedGroups maxsum maxBrkSum R2 / cpt cand pair bi it d addOv newBB)
  (setq cpt (D_GROUP_CENTER g))
  (setq cand '())
  (foreach pair blocks
    (setq bi (cdr pair))
    (if (D_GROUP_CAN_ADD_BLOCK g bi maxsum maxBrkSum R2)
      (progn
        (setq newBB (if (nth 2 g) (D_BBOX_UNION (nth 2 g) (D_BLOCK_BBOX bi)) (D_BLOCK_BBOX bi)))
        (setq addOv (D_GROUPS_OVERLAP_COST newBB fixedGroups))
        (foreach it bi
          (setq d (D_DIST2D2 (D_ITEM_CENTER it) cpt))
          ;; (overlapCost, distance, baseKey, pair)
          (setq cand (append cand (list (list addOv d (car pair) pair))))
        )
      )
    )
  )
  (vl-sort cand
    (function
      (lambda (a b)
        (if (equal (car a) (car b) 1e-9)
          (< (cadr a) (cadr b))
          (< (car a) (car b))
        )
      )
    )
  )
)

(defun D_FIND_PAIR_BY_BASE (blocks baseKey) (assoc baseKey blocks))

(defun D_REMOVE_PAIR_BY_BASE_ONCE (blocks baseKey / p)
  (setq p (assoc baseKey blocks))
  (if p (D_REMOVE_PAIR_ONCE blocks p) blocks)
)

(defun D_EXPAND_GROUP_BY_NEAR_ITEMS (g blocks fixedGroups maxsum maxBrkSum R2 / changed cand one baseKey pair bi)
  (setq changed T)
  (while changed
    (setq changed nil)
    (setq cand (D_BUILD_CAND_ITEM_LIST blocks g fixedGroups maxsum maxBrkSum R2))
    (foreach one cand
      (if (not changed)
        (progn
          (setq baseKey (nth 2 one))
          (setq pair (D_FIND_PAIR_BY_BASE blocks baseKey))
          (if pair
            (progn
              (setq bi (cdr pair))
              (if (D_GROUP_CAN_ADD_BLOCK g bi maxsum maxBrkSum R2)
                (progn
                  (setq g (D_GROUP_ADD_BLOCK g bi))
                  (setq blocks (D_REMOVE_PAIR_BY_BASE_ONCE blocks baseKey))
                  (setq changed T)
                )
              )
            )
          )
        )
      )
    )
  )
  (list g blocks)
)

(defun D_BUILD_ONE_GROUP_AROUND_BASE (blocks fixedGroups maxsum maxBrkSum R2 / g seed bi res)
  (setq g (D_GROUP_INIT_EMPTY))
  (setq seed (D_PICK_SEED_BLOCK_FIT_FIRST blocks maxsum maxBrkSum R2 fixedGroups))
  (if (null seed) (setq seed (D_PICK_LARGEST_BLOCK blocks)))

  (setq blocks (D_REMOVE_PAIR_ONCE blocks seed))
  (setq bi (cdr seed))

  (if (D_GROUP_CAN_ADD_BLOCK g bi maxsum maxBrkSum R2)
    (progn
      (setq g (D_GROUP_ADD_BLOCK g bi))
      (setq res (D_EXPAND_GROUP_BY_NEAR_ITEMS g blocks fixedGroups maxsum maxBrkSum R2))
      (list (D_GROUP_LABEL (car res)) (cadr res))
    )
    (progn
      (setq g (D_GROUP_ADD_BLOCK g bi))
      (list (D_GROUP_LABEL g) blocks)
    )
  )
)

(defun D_ASSIGN_ALL_ITEMS_AROUND_BASE (items maxsum maxBrkSum R2 / blocks groups pair g guard)
  ;; ✅ 변경: BASE는 유지하되, 한도 초과 BASE는 자동 분해해서 분리 허용
  (setq blocks (D_BASEBLOCKS_FLEX items maxsum maxBrkSum))
  (setq groups '())
  (setq guard 0)
  (while (and blocks (< guard 50000))
    (setq guard (1+ guard))
    ;; 이미 확정된 groups와의 겹침이 최소가 되도록 다음 그룹을 확장
    (setq pair (D_BUILD_ONE_GROUP_AROUND_BASE blocks groups maxsum maxBrkSum R2))
    (setq g (car pair))
    (setq blocks (cadr pair))
    (setq groups (append groups (list g)))
  )
  groups
)


;; ============================================================
;; 6) Convex Hull polygon
;; ============================================================

(defun D_CROSSZ (o a b)
  (- (* (- (car a) (car o)) (- (cadr b) (cadr o)))
     (* (- (cadr a) (cadr o)) (- (car b) (car o))))
)

(defun D_PT_LESS (p q / eps)
  (setq eps 1e-9)
  (cond
    ((> (abs (- (car p) (car q))) eps) (< (car p) (car q)))
    (T (< (cadr p) (cadr q)))
  )
)

(defun D_UNIQUE_PTS (pts / out)
  (setq out '())
  (foreach p pts
    (if (not (vl-some '(lambda (q) (equal p q 1e-9)) out))
      (setq out (cons p out))
    )
  )
  (reverse out)
)

(defun D_CONVEX_HULL (pts / s lower upper p)
  (setq pts (D_UNIQUE_PTS pts))
  (if (< (length pts) 3)
    pts
    (progn
      (setq s (vl-sort pts 'D_PT_LESS))

      (setq lower '())
      (foreach p s
        (while (and (>= (length lower) 2)
                    (<= (D_CROSSZ (nth (- (length lower) 2) lower)
                                  (nth (- (length lower) 1) lower)
                                  p)
                        0.0))
          (setq lower (reverse (cdr (reverse lower))))
        )
        (setq lower (append lower (list p)))
      )

      (setq upper '())
      (foreach p (reverse s)
        (while (and (>= (length upper) 2)
                    (<= (D_CROSSZ (nth (- (length upper) 2) upper)
                                  (nth (- (length upper) 1) upper)
                                  p)
                        0.0))
          (setq upper (reverse (cdr (reverse upper))))
        )
        (setq upper (append upper (list p)))
      )

      (setq lower (reverse (cdr (reverse lower))))
      (setq upper (reverse (cdr (reverse upper))))
      (append lower upper)
    )
  )
)

(defun D_BB_CORNERS2D (bb / mn mx)
  (setq mn (car bb) mx (cadr bb))
  (list
    (list (car mn) (cadr mn))
    (list (car mx) (cadr mn))
    (list (car mx) (cadr mx))
    (list (car mn) (cadr mx))
  )
)

(defun D_POLY_CENTER2D (pts / cx cy n)
  (setq cx 0.0 cy 0.0)
  (setq n (float (length pts)))
  (foreach p pts
    (setq cx (+ cx (car p)))
    (setq cy (+ cy (cadr p)))
  )
  (if (> n 0.0)
    (list (/ cx n) (/ cy n))
    (list 0.0 0.0)
  )
)

;; ============================================================
;; 7) Entity creators
;; ============================================================

(defun D_MAKE_POLYLINE_2D_CLOSED (pts lay aci / e dxf p)
  (setq dxf
    (list
      (cons 0 "LWPOLYLINE")
      (cons 100 "AcDbEntity")
      (cons 8 lay)
      (cons 62 aci)
      (cons 100 "AcDbPolyline")
      (cons 90 (length pts))
      (cons 70 1)
    )
  )
  (foreach p pts
    (setq dxf (append dxf (list (cons 10 (list (car p) (cadr p)))))))
  (setq e (entmakex dxf))
  (if (D_ENAMEP e) e (entlast))
)

(defun D_DRAW_GROUP_POLY_CHECK (items lay aci / pts it bb hull)
  (setq pts '())
  (foreach it items
    (setq bb (nth 2 it))
    (setq pts (append pts (D_BB_CORNERS2D bb)))
  )
  (setq hull (D_CONVEX_HULL pts))
  (if (and hull (>= (length hull) 3))
    (D_MAKE_POLYLINE_2D_CLOSED hull lay aci)
  )
  hull
)

(defun D_MAKE_TEXT_CENTER (pt h txt lay aci / e)
  (setq e
    (entmakex
      (list
        (cons 0 "TEXT")
        (cons 10 pt)
        (cons 11 pt)
        (cons 40 h)
        (cons 1 txt)
        (cons 7 "STANDARD")
        (cons 8 lay)
        (cons 62 aci)
        (cons 72 1)
        (cons 73 2)
        (cons 6 "BYLAYER")
      )
    )
  )
  (if (D_ENAMEP e) e (entlast))
)

(defun D_MAKE_TEXT_LEFT (pt h txt lay aci / e)
  (setq e
    (entmakex
      (list
        (cons 0 "TEXT")
        (cons 10 pt)
        (cons 11 pt)
        (cons 40 h)
        (cons 1 txt)
        (cons 7 "STANDARD")
        (cons 8 lay)
        (cons 62 aci)
        (cons 72 0)
        (cons 73 2)
        (cons 6 "BYLAYER")
      )
    )
  )
  (if (D_ENAMEP e) e (entlast))
)

;; ============================================================
;; 8) Draw group + header + set colors
;; ✅ BRK 오른편에 MaxSum/MaxBrkSum/R 출력 추가
;; ============================================================

(defun D_DRAW_GROUP_CHECK (groups headerH lay maxsum maxBrkSum R
                           / gNo g sum bb items baseLabel hull c2d pt gname sumBrk aci it en)

  (setq gNo 1)
  (foreach g groups
    (setq sum       (nth 0 g))
    (setq bb        (nth 2 g))
    (setq items     (nth 3 g))
    (setq baseLabel (nth 4 g))
    (setq sumBrk    (nth 5 g))
    (setq gname     (D_GNAME gNo))
    (setq aci       (D_COLOR_FOR_IDX gNo))

    (foreach it items
      (setq en (car it))
      (D_ENT_SET_COLOR en aci)
    )

    (setq hull (D_DRAW_GROUP_POLY_CHECK items lay aci))
    (if (and hull (>= (length hull) 3))
      (setq c2d (D_POLY_CENTER2D hull))
      (setq c2d (D_BBOX_CENTER2D bb))
    )
    (setq pt (list (car c2d) (cadr c2d) 0.0))

    (D_MAKE_TEXT_CENTER pt headerH
      (strcat gname
              "  SUM=" (rtos sum 2 2)
              "  CNT=" (itoa (length items))
              "  BASE=" baseLabel
              "  BRK=" (rtos sumBrk 2 2)
              "  MaxSum=" (rtos maxsum 2 2)
              "  MaxBrkSum=" (rtos maxBrkSum 2 2)
              "  R=" (rtos R 2 2)
      )
      lay aci
    )

    (setq gNo (1+ gNo))
  )
)

;; ============================================================
;; 9) Listing output
;; ✅ 목록 요약줄도 BRK 오른편에 MaxSum/MaxBrkSum/R 출력 추가
;; ============================================================

(defun D_OUTPUT_LIST_TEXT (groups basept textH lineGap lay maxsum maxBrkSum R
                           / x0 y gNo g sum items it en s xGap baseLabel gname sumBrk aci)

  (setq x0 (car basept))
  (setq y  (cadr basept))
  (setq gNo 1)
  (setq xGap (* 12.0 textH))

  (foreach g groups
    (setq sum       (nth 0 g))
    (setq items     (nth 3 g))
    (setq baseLabel (nth 4 g))
    (setq sumBrk    (nth 5 g))
    (setq gname     (D_GNAME gNo))
    (setq aci       (D_COLOR_FOR_IDX gNo))

    (D_MAKE_TEXT_LEFT (list x0 y 0.0) textH gname lay aci)

    (D_MAKE_TEXT_LEFT (list (+ x0 xGap) y 0.0) textH
      (strcat "SUM=" (rtos sum 2 2)
              "  CNT=" (itoa (length items))
              "  BASE=" baseLabel
              "  BRK=" (rtos sumBrk 2 2)
              "  MaxSum=" (rtos maxsum 2 2)
              "  MaxBrkSum=" (rtos maxBrkSum 2 2)
              "  R=" (rtos R 2 2)
      )
      lay aci
    )
    (setq y (- y lineGap))

    (foreach it items
      (setq en (car it))
      (setq s (D_GET_TEXT en))
      (if (null s) (setq s ""))
      (D_MAKE_TEXT_LEFT (list (+ x0 (* 2.0 textH)) y 0.0) textH s lay aci)
      (setq y (- y lineGap))
    )

    (setq y (- y lineGap))
    (setq gNo (1+ gNo))
  )
)

;; ============================================================
;; 10) Command: CAPREC
;; ============================================================

(defun c:CAPREC (/ ss maxsum maxBrkSum R R2 headerH idx e s cap bb base brk
                 items groups ok skip listPt listH lineGap oldLayer)

  (D_LAYER_ENSURE *CAPREC_LAYER*)

  (setq ss (ssget '((0 . "TEXT,MTEXT"))))
  (if (null ss)
    (progn (princ "\n선택된 TEXT/MTEXT가 없습니다.") (princ))
    (progn
      (setq maxsum    (D_GETREAL_PERSIST "그룹 합산용량 한도(MaxSum) 입력" "MAXSUM" 296.0))
      (setq maxBrkSum (D_GETREAL_PERSIST "그룹 차단기사이즈 합 한도(MaxBrkSum) 입력" "MAXBRKSUM" 1400.0))
      (if (<= maxBrkSum 0.0) (setq maxBrkSum 1400.0) (D_REG_SET_REAL "MAXBRKSUM" maxBrkSum))

      (setq R (D_GETREAL_PERSIST "그룹핑 반지름(R) 입력" "RADIUS" 35000.0))
      (if (<= R 0.0) (setq R 35000.0) (D_REG_SET_REAL "RADIUS" R))
      (setq R2 (* R R))

      (setq headerH (D_GETREAL_PERSIST "폴리곤 헤더(TEXT) 높이 입력" "HEADERH" 300.0))

      (setq items '() ok 0 skip 0)
      (setq idx 0)
      (while (< idx (sslength ss))
        (setq e (ssname ss idx))
        (setq s (D_GET_TEXT e))
        (setq cap (D_PARSE_CAP s))
        (setq brk (D_PARSE_BRK_SIZE s))
        (setq bb  (D_GET_BBOX e))
        (setq base (D_PARSE_BASE s))

        (if (and cap brk bb (/= base ""))
          (progn
            (setq items (cons (list e cap bb base brk) items))
            (setq ok (1+ ok))
          )
          (setq skip (1+ skip))
        )
        (setq idx (1+ idx))
      )
      (setq items (reverse items))

      (if (null items)
        (progn
          (princ "\n파싱 가능한 형식(';')이 없거나 BBOX를 얻지 못했습니다.")
          (princ)
        )
        (progn
          (setq groups (D_ASSIGN_ALL_ITEMS_AROUND_BASE items maxsum maxBrkSum R2))

          (setq oldLayer (D_SET_CUR_LAYER *CAPREC_LAYER*))

          ;; ✅ 헤더 텍스트에 MaxSum/MaxBrkSum/R 포함
          (D_DRAW_GROUP_CHECK groups headerH *CAPREC_LAYER* maxsum maxBrkSum R)

          ;; 목록 출력(영구메모리)
          (setq listPt (getpoint "\n그룹핑 목록(TEXT) 출력 기준점 지정 <Enter=생략>: "))
          (if listPt
            (progn
              (setq listH (D_GETREAL_PERSIST "목록 TEXT 높이 입력" "LISTH" 100.0))
              (setq lineGap (D_GETREAL_PERSIST "목록 줄간격(거리) 입력" "LINEGAP" (* 1.5 listH)))
              ;; ✅ 목록 요약에도 MaxSum/MaxBrkSum/R 포함
              (D_OUTPUT_LIST_TEXT groups listPt listH lineGap *CAPREC_LAYER* maxsum maxBrkSum R)
            )
          )

          (if oldLayer (setvar "CLAYER" oldLayer))

          ;; 명령창 로그(기존 유지)
          (princ
            (strcat
              "\n[CAPREC] 선택=" (itoa (sslength ss))
              " / OK=" (itoa ok)
              " / SKIP=" (itoa skip)
              " / 그룹=" (itoa (length groups))
              " / MaxSum=" (rtos maxsum 2 2)
              " / MaxBrkSum=" (rtos maxBrkSum 2 2)
              " / R=" (rtos R 2 2)
              " / 팔레트=48색(10..240,11..241)"
              "  (구분자=';' ONLY / 레이어=" *CAPREC_LAYER* ")"
            )
          )
        )
      )
    )
  )
  (princ)
)


;; ============================================================
;; RECAPREC: "수정된 출력결과(목록 텍스트)"만으로
;;          그룹 재구성 -> 폴리라인 재생성 -> 재출력(헤더/목록)
;;
;; 중복 텍스트 대응:
;;  1) (그룹 대표색 ACI + 텍스트) 우선 매칭
;;  2) 실패시 (텍스트) fallback 매칭
;;  3) fallback으로 잡힌 엔티티는 새 그룹 ACI로 자동 재색칠
;;
;; 출력 색상:
;;  - 폴리라인, 헤더, 목록 모두 그룹 ACI와 동일
;; ============================================================

(defun D_TRIM2 (s) (vl-string-trim " \t\r\n" (if s s "")))

(defun D_STR_STARTS_WITH (s prefix)
  (and s prefix
       (<= (strlen prefix) (strlen s))
       (= (substr s 1 (strlen prefix)) prefix))
)

(defun D_IS_GNAME_LINE (s / ss n)
  (setq ss (D_TRIM2 s))
  (and (D_STR_STARTS_WITH ss "PP-")
       (= (strlen ss) 7)
       (setq n (atoi (substr ss 4 4)))
       (>= n 0))
)

(defun D_IS_SUMMARY_LINE (s)
  (setq s (D_TRIM2 s))
  (or (D_STR_STARTS_WITH s "SUM=")
      (D_STR_STARTS_WITH s "CNT=")
      (D_STR_STARTS_WITH s "BASE=")
      (D_STR_STARTS_WITH s "BRK=")
      (D_STR_STARTS_WITH s "MaxSum=")
      (D_STR_STARTS_WITH s "MaxBrkSum=")
      (D_STR_STARTS_WITH s "R="))
)

(defun D_ENT_INS_PT_2D (e / ed p)
  (setq ed (entget e))
  (setq p (cdr (assoc 10 ed)))
  (if (and p (listp p))
    (list (car p) (cadr p))
    (list 0.0 0.0)
  )
)

(defun D_SORT_ENTS_BY_Y_DESC (lst /)
  (vl-sort lst
    (function
      (lambda (a b)
        (> (cadr (D_ENT_INS_PT_2D a)) (cadr (D_ENT_INS_PT_2D b)))
      )
    )
  )
)

(defun D_SS_TO_LIST (ss / i out)
  (setq out '() i 0)
  (while (< i (sslength ss))
    (setq out (cons (ssname ss i) out))
    (setq i (1+ i))
  )
  (reverse out)
)

(defun D_ENT_GET_ACI (e / ed v)
  (setq ed (entget e))
  (setq v (cdr (assoc 62 ed)))
  (if v v 256)
)

(defun D_SAFE_ACI (aci fallback)
  (if (and aci (numberp aci) (> aci 0) (/= aci 256))
    aci
    fallback
  )
)

(defun D_MAKE_KEY_ACI_TEXT (aci txt)
  (strcat (itoa aci) "|" txt)
)

;; ----------------------------
;; ASSOC MAP 유틸
;; ----------------------------

(defun D_MAP_ADD (mp key val / pair)
  (if (setq pair (assoc key mp))
    (subst (cons key (append (cdr pair) (list val))) pair mp)
    (append mp (list (cons key (list val))))
  )
)

(defun D_MAP_POP_ONE (mp key / pair lst got)
  (setq pair (assoc key mp))
  (if (null pair)
    (list nil mp)
    (progn
      (setq lst (cdr pair))
      (if (null lst)
        (list nil mp)
        (progn
          (setq got (car lst))
          (setq lst (cdr lst))
          (if lst
            (list got (subst (cons key lst) pair mp))
            (list got (vl-remove pair mp))
          )
        )
      )
    )
  )
)

(defun D_LIST_REMOVE_FIRST (lst target / out removed x)
  (setq out '() removed nil)
  (foreach x lst
    (if (and (null removed) (equal x target))
      (setq removed T)
      (setq out (append out (list x)))
    )
  )
  out
)

(defun D_MAP_REMOVE_ENTITY (mp key en / pair lst newlst)
  (setq pair (assoc key mp))
  (if (null pair)
    mp
    (progn
      (setq lst (cdr pair))
      (setq newlst (D_LIST_REMOVE_FIRST lst en))
      (if (= (length newlst) (length lst))
        mp
        (if newlst
          (subst (cons key newlst) pair mp)
          (vl-remove pair mp)
        )
      )
    )
  )
)

;; ----------------------------
;; 1) 수정된 출력목록 텍스트에서 그룹 파싱
;;   결과: ( (gname gAci (line1 line2 ...)) ... )
;; ----------------------------

(defun D_PARSE_GROUPS_FROM_EDITED_LIST (ents / sorted groups curG curAci curItems e s)
  (setq sorted (D_SORT_ENTS_BY_Y_DESC ents))
  (setq groups '())
  (setq curG nil)
  (setq curAci 256)
  (setq curItems '())

  (foreach e sorted
    (setq s (D_GET_TEXT e))
    (if (null s) (setq s ""))
    (setq s (D_TRIM2 s))

    (cond
      ((= s "") nil)

      ((D_IS_GNAME_LINE s)
       (if curG
         (setq groups (append groups (list (list curG curAci (reverse curItems)))))
       )
       (setq curG s)
       (setq curAci (D_ENT_GET_ACI e))
       (setq curItems '())
      )

      ((D_IS_SUMMARY_LINE s) nil)

      (T
       (if curG (setq curItems (cons s curItems)))
      )
    )
  )

  (if curG
    (setq groups (append groups (list (list curG curAci (reverse curItems)))))
  )
  groups
)

;; ----------------------------
;; 2) 원본 대상(사용자 선택)으로 매칭용 맵 생성
;; ----------------------------

(defun D_BUILD_MATCH_MAPS (ssTarget / i e s aci mpAT mpT lay)
  (setq mpAT '())
  (setq mpT  '())
  (setq i 0)

  (while (< i (sslength ssTarget))
    (setq e (ssname ssTarget i))
    (setq lay (cdr (assoc 8 (entget e))))

    (if (/= lay *CAPREC_LAYER*)
      (progn
        (setq s (D_GET_TEXT e))
        (if (null s) (setq s ""))
        (setq s (D_TRIM2 s))
        (if (/= s "")
          (progn
            (setq aci (D_ENT_GET_ACI e))
            (setq mpT  (D_MAP_ADD mpT s e))
            (setq mpAT (D_MAP_ADD mpAT (D_MAKE_KEY_ACI_TEXT aci s) e))
          )
        )
      )
    )

    (setq i (1+ i))
  )

  (list mpAT mpT)
)

;; ----------------------------
;; 3) 폴리라인 전용 (Convex Hull)
;; ----------------------------

(defun D_DRAW_GROUP_POLY_ONLY (items lay aci / pts it bb hull)
  (setq pts '())
  (foreach it items
    (setq bb (nth 2 it))
    (if bb
      (setq pts (append pts (D_BB_CORNERS2D bb)))
    )
  )
  (setq hull (D_CONVEX_HULL pts))
  (if (and hull (>= (length hull) 3))
    (D_MAKE_POLYLINE_2D_CLOSED hull lay aci)
  )
  hull
)

(defun D_SUM_ITEMS_CAP (items / s it) (setq s 0.0) (foreach it items (setq s (+ s (cadr it)))) s)
(defun D_SUM_ITEMS_BRK (items / s it) (setq s 0.0) (foreach it items (setq s (+ s (nth 4 it)))) s)

;; ----------------------------
;; 4) 재목록 출력(그룹색 동일)
;; groupsOut: ( (gname items sum sumBrk aci) ... )
;; ----------------------------

(defun D_OUTPUT_LIST_RECAPREC (groupsOut basept textH lineGap lay / x0 y xGap g it s aci)
  (setq x0 (car basept))
  (setq y  (cadr basept))
  (setq xGap (* 10.0 textH))

  (foreach g groupsOut
    (setq aci (nth 4 g))

    (D_MAKE_TEXT_LEFT (list x0 y 0.0) textH (car g) lay aci)

    (D_MAKE_TEXT_LEFT
      (list (+ x0 xGap) y 0.0)
      textH
      (strcat
        "SUM=" (rtos (nth 2 g) 2 2)
        "  CNT=" (itoa (length (nth 1 g)))
        "  BRK=" (rtos (nth 3 g) 2 2)
      )
      lay aci
    )

    (setq y (- y lineGap))

    (foreach it (nth 1 g)
      (setq s (D_GET_TEXT (car it)))
      (if (null s) (setq s ""))
      (D_MAKE_TEXT_LEFT (list (+ x0 (* 2.0 textH)) y 0.0) textH s lay aci)
      (setq y (- y lineGap))
    )

    (setq y (- y lineGap))
  )
)

;; ============================================================
;; Command: RECAPREC
;; ============================================================

(defun c:RECAPREC (/ ssEdit ents parsed
                   ssTarget maps mpAT mpT
                   g outGroups missing warn
                   gname gAci gLines
                   headerH listPt listH lineGap
                   oldLayer
                   items itText key popRes en en2
                   cap brk base bb
                   hull c2d pt
                   aciUse polyCnt)

  (D_LAYER_ENSURE *CAPREC_LAYER*)
  (setq oldLayer (D_SET_CUR_LAYER *CAPREC_LAYER*))

  (princ "\n[RECAPREC] 1) 수정된 '목록 출력 텍스트' 전체를 선택하세요. (PP-0001 ~ 항목줄들)")
  (setq ssEdit (ssget '((0 . "TEXT,MTEXT"))))
  (if (null ssEdit)
    (progn
      (princ "\n선택이 없습니다. 종료합니다.")
      (if oldLayer (setvar "CLAYER" oldLayer))
      (princ)
    )
    (progn
      (setq ents (D_SS_TO_LIST ssEdit))
      (setq parsed (D_PARSE_GROUPS_FROM_EDITED_LIST ents))

      (if (or (null parsed) (= (length parsed) 0))
        (progn
          (princ "\nPP-xxxx 그룹 헤더를 찾지 못했습니다. (PP-0001 같은 줄이 있어야 합니다)")
          (if oldLayer (setvar "CLAYER" oldLayer))
          (princ)
        )
        (progn
          (princ
            (strcat
              "\n[RECAPREC] 2) 폴리라인을 그릴 '원본 TEXT/MTEXT'를 선택하세요."
              "\n          (편집한 목록 텍스트는 선택하지 마세요!)"
            )
          )
          (setq ssTarget (ssget '((0 . "TEXT,MTEXT"))))
          (if (null ssTarget)
            (progn
              (princ "\n원본 대상 선택이 없습니다. 종료합니다.")
              (if oldLayer (setvar "CLAYER" oldLayer))
              (princ)
            )
            (progn
              (setq maps (D_BUILD_MATCH_MAPS ssTarget))
              (setq mpAT (car maps))
              (setq mpT  (cadr maps))

              (setq outGroups '())
              (setq missing '())
              (setq warn '())
              (setq polyCnt 0)

              (setq headerH (D_GETREAL_PERSIST "재출력 헤더(TEXT) 높이" "HEADERH" 300.0))

              (foreach g parsed
                (setq gname  (car g))
                (setq gAci   (cadr g))
                (setq gLines (caddr g))

                (setq aciUse (D_SAFE_ACI gAci (D_COLOR_FOR_IDX (1+ (length outGroups)))))

                (setq items '())

                (foreach itText gLines
                  (setq itText (D_TRIM2 itText))

                  ;; 1) (ACI|TEXT) 우선
                  (setq key (D_MAKE_KEY_ACI_TEXT aciUse itText))
                  (setq popRes (D_MAP_POP_ONE mpAT key))
                  (setq en (car popRes))
                  (setq mpAT (cadr popRes))

                  (if (null en)
                    (progn
                      ;; 2) fallback: TEXT만
                      (setq popRes (D_MAP_POP_ONE mpT itText))
                      (setq en2 (car popRes))
                      (setq mpT (cadr popRes))
                      (setq en en2)

                      ;; fallback으로 찾았으면, 그 엔티티를 새 색으로 정리
                      (if (and en (/= aciUse 256))
                        (D_ENT_SET_COLOR en aciUse)
                      )
                    )
                  )

                  (if (null en)
                    (setq missing (cons (strcat gname " :: " itText) missing))
                    (progn
                      (setq cap  (D_PARSE_CAP itText))      (if (null cap) (setq cap 0.0))
                      (setq brk  (D_PARSE_BRK_SIZE itText)) (if (null brk) (setq brk 0.0))
                      (setq base (D_PARSE_BASE itText))
                      (setq bb   (D_GET_BBOX en))

                      (if bb
                        (progn
                          (if (/= aciUse 256) (D_ENT_SET_COLOR en aciUse))
                          (setq items (append items (list (list en cap bb base brk))))
                        )
                        (setq missing (cons (strcat gname " :: [BBOXFAIL] " itText) missing))
                      )
                    )
                  )
                )

                (cond
                  ((< (length items) 1)
                   (setq warn (cons (strcat gname " :: 매칭된 객체 0개 -> 폴리라인 생략") warn))
                  )
                  ((< (length items) 2)
                   (setq warn (cons (strcat gname " :: 객체 1개 -> 폴리라인 생략") warn))
                  )
                  (T
                   (setq hull (D_DRAW_GROUP_POLY_ONLY items *CAPREC_LAYER* aciUse))
                   (if (or (null hull) (< (length hull) 3))
                     (setq warn (cons (strcat gname " :: Hull 점 < 3 -> 폴리라인 생성 실패") warn))
                     (setq polyCnt (1+ polyCnt))
                   )
                  )
                )

                ;; 헤더는 항상 출력(그룹색 동일)
                (if (and items (D_UNION_BBOX_ALL items))
                  (progn
                    (setq c2d (D_BBOX_CENTER2D (D_UNION_BBOX_ALL items)))
                    (setq pt (list (car c2d) (cadr c2d) 0.0))
                    (D_MAKE_TEXT_CENTER pt headerH
                      (strcat gname
                              "  SUM=" (rtos (D_SUM_ITEMS_CAP items) 2 2)
                              "  CNT=" (itoa (length items))
                              "  BRK=" (rtos (D_SUM_ITEMS_BRK items) 2 2))
                      *CAPREC_LAYER* aciUse
                    )
                  )
                )

                ;; outGroups 저장(목록 재출력용, 색 포함)
                (setq outGroups
                  (append outGroups
                    (list (list gname items (D_SUM_ITEMS_CAP items) (D_SUM_ITEMS_BRK items) aciUse))
                  )
                )
              )

              (setq listPt (getpoint "\n[RECAPREC] 검증용 목록 재출력 기준점 <Enter=생략>: "))
              (if listPt
                (progn
                  (setq listH (D_GETREAL_PERSIST "목록 TEXT 높이" "LISTH" 100.0))
                  (setq lineGap (D_GETREAL_PERSIST "목록 줄간격(거리)" "LINEGAP" (* 1.5 listH)))
                  (D_OUTPUT_LIST_RECAPREC outGroups listPt listH lineGap *CAPREC_LAYER*)
                )
              )

              (if oldLayer (setvar "CLAYER" oldLayer))

              (princ
                (strcat
                  "\n[RECAPREC] 그룹=" (itoa (length outGroups))
                  " / 폴리라인생성=" (itoa polyCnt)
                  " / 누락=" (itoa (length missing))
                  "  (중복대응: ACI+TEXT 우선, TEXT fallback)"
                )
              )

              (if warn
                (progn
                  (princ "\n[RECAPREC][폴리라인 경고]")
                  (foreach s (reverse warn) (princ (strcat "\n - " s)))
                )
              )

              (if missing
                (progn
                  (princ "\n[RECAPREC][누락/매칭실패 목록]")
                  (foreach s (reverse missing) (princ (strcat "\n - " s)))
                )
              )
            )
          )
        )
      )
    )
  )
  (princ)
)

(princ)
