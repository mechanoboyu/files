;**************************************************************************;
; 関数名：bd_insert
; ファイル名：batch-insert-multiple-dwg-model.lsp
; 作成日：2023/1/3
; 作成：Noboyu
;
; 内容：複数のDwgファイルのモデル空間を、一括で1枚の図面に集めて、並べます。
;
; 特徴：* 指定したフォルダ内のDwgファイルを読み取り、新規図面のモデル空間に、まとめてinsertします。
;       * 並び方は、100mm間隔で2行n列です。
;
; 開発環境：AutoCAD 2023 Windows版
;
; 注記：  1. 大量の図面を一度に読み込むと、フリーズする可能性があります。
;            使用される際は、元図はバックアップの上、まずは簡単な1〜2枚程度の図面でテストして下さい。
;         2. このコードを保存する際は、必ずエンコードを「Shift-Jis」に指定して下さい。
;         3. 複数の図面同士において、形状違いでブロック名が同じ場合の共用防止のため、
;            ブロック名が変わる仕様です。
;         4. 詳しい内容、使い方は、下記WEBサイトをご覧下さい。
;            https://www.noboyu.com/lisp-batch-insert-drawing/
; 
;**************************************************************************;
(vl-load-com)

(defun *error* (msg)
  (setq testlist nil)
  (setq newFilelist nil)
  (vla-Delete blockRefObj)
  (vla-PurgeAll doc)
  (vla-ZoomAll acadObj)
  (princ msg)
  (princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 匿名ブロックを通常ブロックに変換し、アスタリスクを名前に使わないようにする
(defun convUnnamedBlk () 
  (if (wcmatch oldBlkName "`**")
    (progn 
      (setq oldBlkName (vl-string-subst "temp" "*" oldBlkName))
      (vla-ConvertToStaticBlock listelm oldBlkName)
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ブロック名が重複している際の上書き回避のため、ブロック名を変更する
(defun renameBlk (i / explodedObjects newBlkNameList)
  (setq explodedObjects (vlax-variant-value (vla-Explode blockrefobj)))
  (setq el (vlax-safearray->list explodedObjects))
  (foreach each-item el 
    (if (wcmatch (vl-princ-to-string each-item) "*BlockRef*") 
      (progn 
        (setq listelm each-item)
        (setq oldBlkName (vlax-get-property listelm 'Name))
        (convUnnamedBlk);匿名ブロックを通常ブロックへ
        (setq newBlkName (strcat oldBlkName "-" (rtos i)))
        (setq newBlkNameList (cons newBlkName newBlkNameList))
        ;同一図面内では、ブロック名の変更は一度だけ行う
        (if (null (member oldBlkName newBlkNameList)) 
          (command-s "-rename" "B" oldBlkName newBlkName)
        )
      )
    )
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 対象のDwgデータのリストを順次読み取り、insertメソッドで配置する
(defun setDWG()
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
  (defun Getbd()
    (setq bbox (vl-catch-all-apply 'vla-getboundingbox (list blockrefobj 'point1 'point2 )))
    (if (vl-catch-all-error-p bbox) 
      (progn 
        (alert 
          (strcat "Exception: " 
                  (vl-catch-all-error-message bbox)
                  "¥n 無限の構築線など、取得できない図形が存在する可能性があります"
          )
        )
        (exit)
      )
    )
    (setq minP (vlax-safearray->list point1))
    (setq maxP (vlax-safearray->list point2))
    )

;;;; Main program ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:bd_insert( / blockrefobj)
  ;AutoCAD Applicationオブジェクトへの接続を確立する
  (setq acadObj (vlax-get-acad-object))
  ;現在のDocumentオブジェクトへの接続を確立する
  (setq doc (vla-get-ActiveDocument acadObj))
  (setq modelSpace (vla-get-ModelSpace doc))
  
  (vla-PurgeAll doc)

  (setq gloc (getfiled "対象図面の保存場所のファイルを選択:" "E:¥¥" "" 16))
  (setq loc (vl-filename-directory gloc))
  (setq f-list (vl-directory-files loc "*.dwg"))
  ;フルパスの図面リストをつくる
  (foreach n f-list (setq newFilelist (cons (strcat loc "¥¥" n) newFilelist)))
  ;フルパスの図面リストをソートして、昇順のインデックス番号を取得しておく
  (setq newFilelist-i (vl-sort-i f-list '>))
  ; ファイル数を取得する
  (setq number (length f-list))

  ;初回配置位置
  (setq insertionPnt (vlax-3d-point 0 0 0))

  ;実行前に最終確認を行う
  (initget 1 "Yes No")
  (setq answer (getkword (strcat (rtos number)" 個のファイルを読み込みます。よろしいですか？[Yes/No] <Yes>: ")))
  (cond
   ((= answer "No") (exit))
  )

  (setq index 0)
  (repeat number ;ファイルの数だけ、処理を繰り返す
    ;図面を配置する関数
    (setDWG)
    (prompt (strcat "¥n" "No. "(rtos index)" のファイル"  (nth (nth index newFilelist-i) newFilelist) " です。¥n"))

    (Getbd);境界点座標取得
    (vla-Move blockrefobj (vlax-3d-point minP) insertionPnt)

    (Getbd);図形を移動後の、境界点の座標を取得

    ; 境界点リスト作成
    (setq testlist (cons minp (cons maxp testlist)))
    
    ; ブロック重複時の上書き回避のため、ブロック名を変更する関数を実行
    (renameBlk index)

    ; 2行n列で並べるため、
    ; 次の配置点は、現在の場所が、2で割り切れるかどうかで判断する
    (setq nextPoint
      (cond
        ((= (rem index 2) 0) (list (car minp) (+ 100 (cadr maxp))))
        (t (list (+ 100 (apply 'max (mapcar 'car testlist))) (apply 'min (mapcar 'cadr testlist))))
      )
    )
    (setq insertionPnt (vlax-3D-point nextPoint))
    (vla-Delete blockRefObj)
    (setq index (1+ index))
  );repeat

  (vla-ZoomAll acadObj)
  ;座標リストを空にしておく
  (setq testlist nil)
  (setq newFilelist nil)
  ;参照されていない名前の付いたオブジェクトを削除する
  (vla-PurgeAll doc)
  (vla-Regen doc acAllViewports)
  (princ)
)
