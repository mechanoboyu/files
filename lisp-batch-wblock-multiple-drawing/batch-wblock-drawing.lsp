;**********************************************************;
; 関数名：bd_wblock
; ファイル名：batch-wblock-drawing.lsp
; 作成日：2022/12/19
; 作成：Noboyu
;
; 内容：モデル空間に書いた複数の図面を、一括で1枚ずつ分割して、
;       1図面1ファイルとして書き出します。
;
; 特徴：* 書き出した図面は、図枠左下が原点に設定されます。
;       * 書き出した図面には、図面範囲が設定されます。（印刷の際などに便利）
;
; 注記：  1. 大量の図面を一度に書き出すと、フリーズする可能性があります。
;            使用される際は、元図はバックアップの上、まずは1〜2枚程度の図面でテストして下さい。
;         2. このコードを保存する際は、エンコードを「Shift-Jis」に指定して下さい。
;         3. 詳しい内容、使い方は、下記WEBサイトをご覧下さい。
;            https://www.noboyu.com/         
; 
;**********************************************************;

(vl-load-com)

(defun *error* (msg)
  (vla-Delete ssetObj)
  (vla-Delete ssetObjdwg)
  (command-s "ucs" "W")
  (princ "エラー: ")
  (princ msg)
  (princ)
)

(defun c:bd_wblock( / acadObj doc modelSpace ssetObj ssetObjdwg gloc loc index cnt number)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;書き出し実行後の画面の表示状態を、書き出し範囲全体にする
  (defun Extpoints( / point1 point2 minP maxP llist rlist)      
    
    (vlax-for entry ssetObj
      (vla-getboundingbox entry 'point1 'point2)
      (setq minP (vlax-safearray->list point1))
      (setq llist (cons minP llist))
      (setq maxP (vlax-safearray->list point2))
      (setq rlist (cons maxP rlist)) 
    )
    ;minPointとmaxPointのリストから、最小と最大の座標を取り出す
    (setq lp (apply 'mapcar (cons 'min llist))
          rp (apply 'mapcar (cons 'max rlist))
    )
    (command-s "zoom" "W" lp rp)
  )
  
;;;; Main program ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;AutoCAD Applicationオブジェクトへの接続を確立する
  (setq acadObj (vlax-get-acad-object))
  ;現在のDocumentオブジェクトへの接続を確立する
  (setq doc (vla-get-ActiveDocument acadObj))
  (setq modelSpace (vla-get-ModelSpace doc))
  ;外枠のレイヤー名を指定
  (setq layerName "Defpoints")
  
  ;実行前に何かが選択されていたら、解除する
  (sssetfirst nil nil)
  
  (prompt "保存先のフォルダ内のファイルを、指定して下さい。¥n（そこからフォルダのパスを取得します）")(terpri)
  
  ; ユーザーが、書き出し先のフォルダに置いているファイルを選択する
  ; そのファイルから、フォルダの場所を読み取る
  (setq gloc (getfiled "保存先のファイルを選択:" "E:¥¥" "" 16))
  (setq loc (vl-filename-directory gloc))
  
  (prompt (strcat "保存先のフォルダの場所は、"  loc " です"))(terpri)
  
  (prompt "図面の外枠を選択して下さい：")(terpri)
  
  ;現在の図面からSelectionSetsコレクションを探して、新しい選択セットに追加する
  (setq ssetObj (vla-Add (vla-get-SelectionSets doc) "frameSET"))
  (setq ssetObjdwg (vla-Add (vla-get-SelectionSets doc) "eachDWG"))
  
  ; 図枠の外枠をユーザーが選択する。
  ; 誤選択防止のため、フィルターあり。条件はポリライン且つ、画層は"Defpoints"
  ; フィルターのタイプ(DXFグループ コード)のための配列を作る
  (setq FilterType (vlax-make-safearray vlax-vbInteger '(0 . 1)))
  (vlax-safearray-fill FilterType (list 0 8))
  ; フィルターの値のための配列を作る
  (setq FilterData (vlax-make-safearray vlax-vbVariant '(0 . 1)))
  (vlax-safearray-fill FilterData (list "LWPOLYLINE" layerName))
  ; ユーザーが範囲を選択する
  (vla-SelectOnScreen ssetObj FilterType FilterData)
  ; 選択した外枠の数を取得する
  (setq number (vla-get-Count ssetObj))  
  
  ;実行前に最終確認を行う
  (initget 1 "Yes No")
  (setq answer (getkword (strcat (rtos number)" 個のファイルを書き出します。よろしいですか？[Yes/No] <Yes>: ")))
  (cond
   ((= answer "No") (exit))
  )
  
  ; 一括WBLOCK開始
  (setq index 0)
  (repeat number ;外枠の数だけ、処理を繰り返す
    ;外枠の対角の点の配列を取得する
    (vla-getboundingbox (vla-Item ssetObj index) 'P1 'P2)
    (setq Pnt2(vlax-safearray->list P2))
    (setq Pnt1(vlax-safearray->list P1))
    
    ;中身を選択するための準備を行う
    ;選択モードを指定（領域内および、交差するオブジェクトを選択する）
    (setq mode acSelectionSetCrossing)
    
    ; 書き出し範囲を全体表示する
    (vla-ZoomWindow acadObj p1 p2)
    ; 書き出す内容（外枠の中身）を選択する
    (vla-Select ssetObjdwg mode P1 P2)

    (prompt (strcat "¥n setObjdwgの要素数は " (rtos (vla-get-Count ssetObjdwg))" 個 ¥n"))
    
    ; 書き出し図面の左下を、原点に設定する
    (command "ucs" Pnt1 "")
    ; 書き出し図面の原点と右上角の座標を取得する
    (setq origin '(0 0 0)
          tp2 (trans Pnt2 0 1)
    )
    ; 図面範囲を設定する
    (command "limits" '(0 0 0) tp2)
    ; 書き出し図面の範囲を全体表示する
    (vla-ZoomWindow acadObj (vlax-3d-point origin) (vlax-3d-point tp2))    
    
    ;WBLOCKを実行
    (vla-Wblock doc (strcat loc "¥¥" "wblock_batch_test_" (rtos (1+ index)) ".dwg") ssetObjdwg)
    (vla-Clear ssetObjdwg)
    (command "ucs" "W")
    (setq index (1+ index))
  );repeat
  
  (Extpoints)
  
  (prompt (strcat "¥n" (rtos index)" 個のファイルを書き出しました。保存先は、"  loc " です"))(terpri)
  (alert (strcat (rtos index)" 個のファイルを書き出しました。¥n 保存先は、"  loc " です"))
  
  (vla-Delete ssetObj)
  (vla-Delete ssetObjdwg)
  (princ)
)
