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
        
      </div> <!-- .btn-group-vertical -->
    </div><!-- .btn-toolbar -->
  </div><!-- #toolbox -->
  
  <div id="canvas-container" class="pull-left">
    <canvas id="edit" width="800" height="600" ng-init="initialize(${ident})" />
  </div>

    </div><!-- controller -->
    
</apply>
