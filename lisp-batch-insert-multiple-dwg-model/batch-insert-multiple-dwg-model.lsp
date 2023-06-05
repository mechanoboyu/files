

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
  (setq oldBlkName (vla-get-EffectiveName listelm))
  ;この辺あやしい
  (if (= :vlax-true (vla-get-isdynamicblock listelm)) 
    (progn 
      (setq oldBlkName (strcat "temp" oldBlkName))
      (vla-ConvertToStaticBlock listelm (strcat "_" oldBlkName))
    )
  )
  (if (wcmatch oldBlkName "*`**") 
    (progn 
      (setq oldBlkName (vl-string-subst "temp" "*" oldBlkName))
      (vla-ConvertToStaticBlock listelm (strcat "_" oldBlkName))
    )
  )
  ;(vla-ConvertToStaticBlock listelm oldBlkName)
  ;(vla-ConvertToStaticBlock listelm (strcat "_" oldBlkName))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ブロック名が重複している際の上書き回避のため、ブロック名を変更する
(defun renameBlk (i / newBlkNameList) 
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
        (if (null (member oldBlkName newBlkNameList))  ;一致しなかったら
          (progn 
            (setq newBlkName (strcat oldBlkName "-" (rtos i)))
            (setq newBlkNameList (cons newBlkName newBlkNameList))
            (command-s "-rename" "B" oldBlkName newBlkName)
          )
        )

        (princ newBlkNameList)
        
        ;同一図面内では、ブロック名の変更は一度だけ行う


      )
    )
  )
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
(defun c:bd_insert () 
  ;AutoCAD Applicationオブジェクトへの接続を確立する
  (setq acadObj (vlax-get-acad-object))
  ;現在のDocumentオブジェクトへの接続を確立する
  (setq doc (vla-get-ActiveDocument acadObj))
  (setq modelSpace (vla-get-ModelSpace doc))

  (vla-PurgeAll doc)
  (vla-PurgeAll doc)
  (setq gloc (getfiled "対象図面の保存場所のファイルを選択:" "E:\\" "" 16))
  (setq loc (vl-filename-directory gloc))
  (setq f-list (vl-directory-files loc "*.dwg"))
  ;フルパスの図面リストをつくる
  (foreach n f-list (setq newFilelist (cons (strcat loc "\\" n) newFilelist)))
  ;フルパスの図面リストをソートして、昇順のインデックス番号を取得しておく
  (setq newFilelist-i (vl-sort-i f-list '>))
  ; ファイル数を取得する
  (setq number (length f-list))

  ;配置位置
  (setq insertionPnt (vlax-3d-point 0 0 0))

  ;作業時と配置用の画層を、新しく作っておく
  (setq layers (vla-get-Layers doc))
  (setq layname "New_Layer")
  (setq tempLayname "Temp_Layer")
  (setq layerObj (vla-Add layers layname))
  (setq layerObj2 (vla-Add layers tempLayname))

  ;実行前に最終確認を行う
  (initget 1 "Yes No")
  (setq answer (getkword 
                 (strcat (rtos number) " 個のファイルを読み込みます。よろしいですか？[Yes/No] <Yes>: ")
               )
  )
  (cond 
    ((= answer "No") (exit))
  )

  (setq index 0)
  (repeat number  ;ファイルの数だけ、処理を繰り返す
    ;作業用の画層をアクティブにする
    (vla-put-ActiveLayer doc layerObj2)
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

    (setq bname (strcat (rtos index) "-" (vla-get-Name blockRefObj)))
    (setq blkcoll (vla-get-blocks doc))
    (setq blk (vla-Add blkcoll (vlax-3d-point minp) bname))

    ; ブロック重複時の上書き回避のため、ブロック名を変更する関数を実行
    (renameBlk index)
    ;関数renameBlkで分解したオブジェクトを集めて、一つのブロックを作る
    (vla-copyobjects doc explodedObjects blk)

    ;配置用の画層をアクティブにする
    (vla-put-ActiveLayer doc layerObj)
    ;先ほど作ったブロックを配置する
    (vla-insertblock modelSpace insertionPnt bname 1 1 1 0)
    (vla-delete blockRefObj)
    ;(vlax-release-object acadObj)
    (setq index (1+ index))
  ) ;repeat

  ;最終的なブロック以外を選択して、削除する
  (setq ssset (ssget "X" 
                     (list (cons -4 "<NOT") 
                           (cons -4 "<AND")
                           (cons 0 "insert")
                           (cons 8 layname)
                           (cons -4 "AND>")
                           (cons -4 "NOT>")
                     )
              )
  )
  (command-s "Erase" ssset "")

  (vla-ZoomAll acadObj)
  ;座標リストを空にしておく
  (setq newFilelist nil)
  ;参照されていない名前の付いたオブジェクトを削除する
  (vla-PurgeAll doc)
  (vla-Regen doc acAllViewports)
  ;(vlax-release-object acadObj)
  (princ)
)