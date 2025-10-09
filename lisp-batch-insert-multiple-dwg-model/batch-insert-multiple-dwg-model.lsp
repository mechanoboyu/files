(vl-load-com)
;**************************************************************************************;
; 関数名：bd_insert
; ファイル名：batch-insert-multiple-dwg-model.lsp
; 作成日：2023/1/3
; 作成：Noboyu
;詳細は、https://www.noboyu.com/lisp-batch-insert-drawing/
; 内容：複数のDwgファイルのモデル空間を、一括で1枚の図面に集めて、並べます。
;2025/9/27:試験的に改変。ピッチをユーザ入力に変更。（未デバッグです。）
;2025/10/5:行列両方のピッチおよび回数を入力可能にした
;2025/10/9：ファイル名のプレフィックスとサフィックス両方考慮する仕様にした
;**************************************************************************************;

(defun *error* (msg) 
  (setq testlist nil)
  (setq newFilelist nil)
  (vla-Delete blockRefObj)
  (vla-PurgeAll doc)
  (vla-ZoomAll acadObj)
  (princ msg)
  (princ)
)

;配置の仕様を、ピッチをユーザーに聞く
(defun askArraySpec () 
  ;行、列方向の数を設定
  (setq m_row (fix (getreal "行数を入力して下さい: ")))
  (while (zerop m_row) 
    (setq m_row (fix (getreal "1 以上の行数を入力して下さい: ")))
  )
  (setq n_col (fix (getreal "列数を入力して下さい: ")))
  (while (zerop n_col) 
    (setq n_col (fix (getreal "1 以上の列数を入力して下さい: ")))
  )
  (princ (strcat "行数を、" (rtos m_row) " に設定しました"))
  (terpri)
  (princ (strcat "列数を、" (rtos n_col) " に設定しました"))
  ;ピッチを設定
  (setq dcol (getreal "\nX方向のピッチを入力してください: "))
  (setq drow (getreal "\nY方向のピッチを入力してください: "))
)

;;
;任意の位置の要素の値を計算する
(defun CalcElment (index pitch) 
  (+ 0 (* index pitch))
)
;nextpointのリストを作る
(defun npList (/ i n xl yl) 
  (setq xl nil)
  (setq yl nil)
  (setq npL nil)
  (setq x_result_list nil)
  (setq y_result_list nil)

  (setq i 0)
  (setq n 0)

  (if (< (length npL) number) 
    (setq m_row (fix (1+ (/ number n_col 1.))))
  )
  (repeat n_col 
    (setq xl (cons (CalcElment n dcol) xl))
    (setq n (1+ n))
  )
  (repeat m_row 
    (setq yl (cons (CalcElment i drow) yl))
    (setq i (1+ i))
  )
  (setq xl (reverse xl))
  (setq yl (reverse yl))
  ;; 1. XLを繰り返すリストを作成
  (setq x_result_list (apply 'append (mapcar '(lambda (x) xl) yl)))

  ;; 2. YLの各要素をXLの要素数分繰り返すリストを作成
  (setq y_result_list (apply 'append 
                             (mapcar 
                               '(lambda (y_val) 
                                  (mapcar '(lambda (x_dummy) y_val) xl)
                                )
                               yl
                             )
                      )
  )
  (setq npL (mapcar 'list x_result_list y_result_list))
)

;ファイル名末尾が一桁の連番が混ざっている場合でも、昇順を維持。
;1,10,2,20ではなく、1,2,10,20になるようにする
;ファイル名の末尾がハイフンで区切られている前提
;aaa-1.dwgなど。
;ハイフンで分離
(defun SplitString (str delim / pos sub_str result) 
  (setq result '())
  (setq sub_str str)
  (while (setq pos (vl-string-search delim sub_str)) 
    (setq result (append result (list (substr sub_str 1 pos))))
    (setq sub_str (substr sub_str (+ pos (strlen delim) 1)))
  )
  (append result (list sub_str))
)
;末尾の連番の数字だけを取得する
(defun get-number-from-filename (s) 
  ((lambda (/ tmp) 
     (setq tmp (SplitString s "-"))
     ;(print tmp)
     (read (last tmp))
   ) 
  )
)
;ソートのため、拡張子を削除
(defun delDWGandSort (/ tmp) 
  (setq tmp (mapcar 
              '(lambda (x) 
                 (vl-string-subst "" ".dwg" (strcase x T))
               )
              f-list
            )
  )

  (vl-sort-i tmp 
             '(lambda (a b) 
                (< (get-number-from-filename a) (get-number-from-filename b))
              )
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ファイル名のソート処理
;拡張子消す
(defun delDWG (/ tmp) 
  (setq tmp (mapcar 
              '(lambda (x) 
                 (vl-string-subst "" ".dwg" (strcase x T))
               )
              f-list
            )
  )
)
;;数字以外は0に変換してリスト出力
(defun extractNUM (filen / tmp) 
  ;ファイルネームをアスキーコードに変換したリストを返す
  (setq temp (mapcar 
               '(lambda (x) 
                  (vl-string->list x)
                )
               filen
             )
  )

  (setq preSuflst (mapcar 
                    '(lambda (x) 
                       (mapcar 
                         '(lambda (y) 
                            (if (or (< 1 y 48) (< 57 y)) 
                              nil
                              y
                            )
                          )
                         x
                       )
                     )
                    temp
                  )
  )
)

  ;;;;抽出した数字の前半と後半を分割
(defun splitLst (l / k i pos ll) 
  (setq prefix nil)
  (setq suffix nil)
  (setq i 0)
  ;前半のリストの処理
  ;prefixがない場合、0を付加 OK
  (setq ll (mapcar 
             '(lambda (x) 
                (if (= (car x) nil) 
                  (cons 0 x);仮の処理
                  x
                )
              )
             l
           )
  )
  ;;;;;;nilを抜いて、前半のprefixだけを抽出する
  (defun extract-non-nil-prefix (sublist) 
    ((lambda (temp-list result) 
       (while (and temp-list (/= (car temp-list) nil)) 
         (setq result (append result (list (car temp-list))))
         (setq temp-list (cdr temp-list))
       )
       result
     ) 
      sublist
      '()
    )
  )
  ;後半リストの処理
  (defun extract-non-nil-suffix (sublist) 
    (setq suf (member nil sublist))
    ;先頭にnilがあるばあい、先頭をリストから削るのを繰り返す
    (if suf 
      (while (and suf (= (car suf) nil)) 
        (setq suf (cdr suf))
      )
    )
    suf
  )
  ;prefixとsuffixをそれぞれ抽出
  (setq prefix (mapcar 'extract-non-nil-prefix ll))
  (setq suffix (mapcar 'extract-non-nil-suffix ll))

  ;;;;アスキーコードを文字に戻す
  (setq preANDsuffix (mapcar 
                       '(lambda (pre suf) 
                          (list (atoi (vl-list->string pre)) 
                                (atoi (vl-list->string suf))
                          ) ;
                        )
                       prefix
                       suffix
                     )
  )
)
  ;;;;リストのcarどうし、cadrどうしをソートする
(defun Sortl (lst / tmp) 
  (vl-sort-i lst 
             (function 
               (lambda (e1 e2) 
                 (cond 
                   ((< (car e1) (car e2)))
                   ((= (car e1) (car e2)) (< (cadr e1) (cadr e2)))
                 )
               )
             )
  )
)
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 匿名ブロックを通常ブロックに変換し、アスタリスクを名前に使わないようにする
(defun convUnnamedBlk () 
  ;ダイナミックブロックだったら実行
  (if (= :vlax-true (vla-get-isdynamicblock listelm)) 
    (progn 
      (setq oldBlkName (strcat "temp" oldBlkName))
      (vla-ConvertToStaticBlock listelm (strcat "st_" oldBlkName "_" (itoa cnt)))
      (setq cnt (1+ cnt))
    )
  )
  ;名前にアスタリスクが入っていたら実行
  (if (wcmatch oldBlkName "*`**") 
    (progn 
      (setq oldBlkName (vl-string-subst "temp" "*" oldBlkName))
      (vla-ConvertToStaticBlock listelm (strcat "_" oldBlkName))
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ブロック名が重複している際の上書き回避のため、ブロック名を変更する
(defun renameBlk (i / explodedObjects newBlkNameList) 
  (setq explodedObjects (vlax-variant-value (vla-Explode blockrefobj)))
  (setq el (vlax-safearray->list explodedObjects))
  ;(princ el)
  (foreach each-item el 
    (if (wcmatch (vl-princ-to-string each-item) "*BlockRef*") 
      (progn 
        (setq listelm each-item)
        (setq oldBlkName (vlax-get-property listelm 'Name))
        (convUnnamedBlk) ;匿名ブロックを通常ブロックへ
        (setq oldBlkName (vlax-get-property listelm 'Name))
        ;同一図面内では、ブロック名の変更は一度だけ行う
        (if (null (member oldBlkName newBlkNameList))  ;既に変更済みでなかったら
          (progn 
            (setq newBlkName (strcat oldBlkName "-" (rtos i)))
            (setq newBlkNameList (cons newBlkName newBlkNameList))
            (command-s "-rename" "B" oldBlkName newBlkName)
          )
        )
        ;(princ newBlkNameList)
      ) ;progn
    ) ;if
  ) ;foreach
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 対象のDwgデータのリストを順次読み取り、insertメソッドで配置する
(defun setDWG () 
  ; ファイルが読み込めないなどでエラーが発生すると、アラートを表示します
  (setq blockRefObj (vl-catch-all-apply 
                      'vla-InsertBlock
                      (list modelSpace 
                            insertionPnt
                            (nth (nth index newFilelist-i) newFilelist)
                            1
                            1
                            1
                            0
                      )
                    )
  )
  (if (vl-catch-all-error-p blockRefObj) 
    (alert (strcat "エラー内容: " (vl-catch-all-error-message blockRefObj)))
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;読み込んだ図形の境界点のmin、max座標を取得する
(defun Getbd () 
  (setq bbox (vl-catch-all-apply 'vla-getboundingbox 
                                 (list blockrefobj 'point1 'point2)
             )
  )
  (if (vl-catch-all-error-p bbox) 
    (progn 
      (alert 
        (strcat "Exception: " 
                (vl-catch-all-error-message bbox)
                "\n 無限の構築線など、取得できない図形が存在する可能性があります"
        )
      )
      (exit)
    )
  )
  (setq minP (vlax-safearray->list point1))
  (setq maxP (vlax-safearray->list point2))
)

;;;; Main program ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:bd_insert (/ blockrefobj) 
  ;AutoCAD Applicationオブジェクトへの接続を確立する
  (setq acadObj (vlax-get-acad-object))
  ;現在のDocumentオブジェクトへの接続を確立する
  (setq doc (vla-get-ActiveDocument acadObj))
  (setq modelSpace (vla-get-ModelSpace doc))

  (vla-PurgeAll doc)



  (askArraySpec)


  (setq gloc (getfiled "対象図面の保存場所のファイルを選択:" "E:\\" "" 16))
  (setq loc (vl-filename-directory gloc))
  (setq f-list (vl-directory-files loc "*.dwg"))

  ;  (prin1 f-list)

  ;フルパスの図面リストをつくる
  (setq newFilelist (mapcar '(lambda (filename) (strcat loc "\\" filename)) 
                            f-list
                    )
  )

  ;ソート準備。数字以外は0に変換してリスト出力
  (extractNUM (delDWG))
  ;;;;抽出した数字の前半と後半を分割
  (splitLst preSuflst)
  ;(prin1 newFilelist)

  ;フルパスの図面リストをソートして、昇順のインデックス番号を取得しておく
  (setq newFilelist-i (Sortl preANDsuffix))


  ; ファイル数を取得する
  (setq number (length f-list))

  ;初回配置位置
  (setq insertionPnt (vlax-3d-point 0 0 0))

  ;実行前に最終確認を行う
  (initget 1 "Yes No")
  (setq answer (getkword 
                 (strcat (rtos number) " 個のファイルを読み込みます。よろしいですか？[Yes/No] <Yes>: ")
               )
  )
  (cond 
    ((= answer "No") (exit))
  )


  ;;配置点のリストを作る
  (npList)
  ;ファイル数を超える行数が入力されていたら、ファイル数をループ回数とする。
  (if (> (length npL) number) (setq number number))

  (setq index 0)
  (repeat number  ;ファイルの数だけ、処理を繰り返す
    ;図面を配置する関数
    (setDWG)
    (prompt 
      (strcat "\n" 
              "No. "
              (rtos index)
              " のファイル"
              (nth (nth index newFilelist-i) newFilelist)
              " です。\n"
      )
    )

    (Getbd) ;境界点座標取得
    (vla-Move blockrefobj (vlax-3d-point minP) insertionPnt)

    (Getbd) ;図形を移動後の、境界点の座標を取得

    ; 境界点リスト作成
    (setq testlist (cons minp (cons maxp testlist)))

    ; ブロック重複時の上書き回避のため、ブロック名を変更する関数を実行
    (setq cnt 0)
    (renameBlk index)

    ; (prompt "\ntestlist：") ;デバッグ用
    ; (prin1 testlist) ;デバッグ用
    ; (prompt "\n\n") ;デバッグ用

    (setq nextPoint (nth (1+ index) npL))

    (prompt "\n次の点：")
    (prin1 nextpoint)
    (prompt "\n")
    (setq insertionPnt (vlax-3D-point nextPoint))
    (vla-Delete blockRefObj)
    (setq index (1+ index))
  ) ;repeat

  (vla-ZoomAll acadObj)
  ;座標リストを空にしておく
  (setq testlist nil)
  (setq newFilelist nil)
  ;参照されていない名前の付いたオブジェクトを削除する
  (vla-PurgeAll doc)
  (vla-Regen doc acAllViewports)
  (princ)
)
