<bind tag="subtitle">作成 - <ident/></bind>
<bind tag="in-header">
  <link rel="stylesheet" href="/css/edit.css" type="text/css" />
  <link rel="stylesheet" href="/css/colorpicker.css" type="text/css" />
  <script src="${jquery.js}"/>
  <script src="/js/fabric.min.js"/>
  <script src="${bootstrap.js}"/>
  <script src="/js/bootstrap-colorpicker.js"/>
  <script src="/js/edit.js"/>
</bind>

<apply template="base">
  
  <div class="clearfix" ng-controller="EditCtrl">

    <!-- Config Modal -->
    <div class="modal fade" id="configModal" tabindex="-1" role="dialog" aria-labelledby="configModalLabel" aria-hidden="true">
      <div class="modal-dialog">
        <div class="modal-content">
          <div class="modal-header">
            <button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>
            <h4 class="modal-title" id="configModalLabel">設定</h4>
          </div>
          <div class="modal-body">
            <div class="checkbox">
              <label>
                <input type="checkbox" ng-model="useAlpha">アルファチャンネルを使う
              </label>
            </div>
          </div>
        </div><!-- /.modal-content -->
      </div><!-- /.modal-dialog -->
    </div><!-- /.modal -->

    <!-- Submit Modal -->
    <div class="modal fade" id="submitModal" tabindex="-1" role="dialog" aria-labelledby="submitModalLabel" aria-hidden="true">
      <div class="modal-dialog">
        <div class="modal-content">
          <div class="modal-header">
            <button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>
            <h4 class="modal-title" id="submitModalLabel">保存</h4>
          </div>
          <div class="modal-body">

            <form class="form-horizontal" role="form" ng-submit="saveImage()">

            <div class="form-group">
              <label for="deletePassword" class="col-sm-4 control-label">削除パスワード</label>
              <div class="col-sm-4">
                <input type="text" class="form-control" id="deletePassword" ng-model="deletePassword" placeholder="1文字以上の文字列">
              </div>
            </div>

              <div class="form-group">
                <div class="col-sm-offset-4 col-sm-8">
                  <div class="checkbox">
                    <label>
                      <input type="checkbox" ng-model="useAlpha">アルファチャンネルを使う
                    </label>
                  </div>
                </div>
              </div>
            </form>

            <div class="modal-footer">
              <button type="button" class="btn btn-default" data-dismiss="modal">キャンセル</button>
              <button type="button" class="btn btn-primary" ng-disabled="deletePassword.length < 1" ng-click="saveImage()">保存</button>
            </div>

          </div>
        </div><!-- /.modal-content -->
      </div><!-- /.modal-dialog -->
    </div><!-- /.modal -->

    

    <div id="toolbox" class="pull-left">
    <div class="btn-toolbar" role="toolbar">
      <div class="btn-group-vertical">

        <button id="submitButton" type="button" ng-click="submit()" ng-disabled="inQuery" class="btn btn-default">
          <span ng-hide="previewShown" class="has-tooltip glyphicon glyphicon-ok"></span>
          <span ng-show="previewShown" class="has-tooltip glyphicon glyphicon-save"></span>
        </button>

        <button id="removeButton" type="button" ng-click="remove()" class="btn btn-default">
          <span class="glyphicon glyphicon-remove"></span>
        </button>
        
        <button id="pickerButton" type="button" ng-click="showPicker()" class="btn btn-default">
          <div id="colorpicker" class="input-append color" data-color="rgb(0,0,0)" data-color-format="rgba">
            <span class="add-on"><i/></span>
          </div>
        </button>

        <button id="configButton" class="btn btn-default" data-toggle="modal" data-target="#configModal">
          <span class="glyphicon glyphicon-wrench"></span>
        </button>
        
      </div> <!-- .btn-group-vertical -->
    </div><!-- .btn-toolbar -->
  </div><!-- #toolbox -->
  
  <div id="canvas-container" class="pull-left">
    <canvas id="edit" width="800" height="600" ng-init="initialize(${ident})" />
  </div>

    </div><!-- controller -->
    
</apply>
