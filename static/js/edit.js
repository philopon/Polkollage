function Editor (canvas_id, image_id, init_color){
  this.canvas = new fabric.Canvas(canvas_id, {
    selection: false
  });

  _this = this;
  this.color = init_color;
  this.preview = null;
  this.onShowPreview = null;
  this.onEndPreview  = null;

  var canvas = this.canvas;
  
  fabric.util.loadImage("/raw/" + image_id, function(imageElement){
    var image = new fabric.Image(imageElement,{
      left: 0,
      top:  0,
      selectable: false
    });
    canvas.add(image);

    var creating_circle;
    canvas.on('mouse:down', function(options){
      if(_this.preview){
        canvas.remove(_this.preview);
        canvas.renderAll();
        _this.preview = null;
        if(_this.onEndPreview){_this.onEndPreview(_this)}
      } else if(!options.target || options.target.type == 'image'){
        var rect  = options.e.target.getBoundingClientRect();
        creating_circle = new fabric.Circle({
          left: options.e.clientX - rect.left,
          top:  options.e.clientY - rect.top,
          radius: 0,
          originX: 'center',
          originY: 'center',
          fill:    _this.color,
          opacity: 0.5
        });
        canvas.add(creating_circle);
      }
    });

    canvas.on('mouse:move', function(options){
      if(creating_circle){
        var rect = options.e.target.getBoundingClientRect()
        ,   left = options.e.clientX - rect.left
        ,   top  = options.e.clientY - rect.top;
        var radius = Math.sqrt(
          Math.pow(creating_circle.left - left, 2) 
          + Math.pow(creating_circle.top - top, 2) 
        )
        var angle = 180 *
        Math.atan2( creating_circle.top - top
                   ,creating_circle.left- left) / Math.PI;
        angle -= 90;
        if(angle < 0) angle += 360;

        creating_circle.setAngle(angle);
        creating_circle.setRadius(radius);
        canvas.renderAll();
      }
    });

    canvas.on('mouse:up', function(options){
      if(creating_circle){
        if(creating_circle.radius < 5){
          creating_circle.setRadius(5);
        }
        creating_circle.setOpacity(1);
        creating_circle.setCoords();
        canvas.setActiveObject(creating_circle);
        canvas.renderAll();
        creating_circle = null;
      }
    });
  });
}

Editor.prototype.getCircles = function(){
  var objects = this.canvas.getObjects();
  var circles = [];
  for(var i = 0; i < objects.length; i++){
    if(objects[i].type == "image") continue;
    circles.push(objects[i]);
  }
  return circles;
}

Editor.prototype.getData = function(){
  var objects = this.canvas.getObjects();
  var data = []
  for(var i = 0; i < objects.length; i++){
    var o = objects[i];
    if(o.type == "image") continue;
    data.push({radius: o.radius,
               scaleX: o.getScaleX(),
               scaleY: o.getScaleY(),
               angle:  o.getAngle(),
               top:    o.getTop(),
               left:   o.getLeft()
               })
  }
  return data;
}

Editor.prototype.removeSelected = function(){
  this.canvas.remove(this.canvas.getActiveObject());
}

Editor.prototype.hasSelected = function(){
  return !!this.canvas.getActiveObject();
}

Editor.prototype.removeAll = function(){
  var cs = this.getCircles();
  for(var i = 0; i < cs.length; i++){
    this.canvas.remove(cs[i]);
  }
}

Editor.prototype.setColor = function(color){
  var cs = this.getCircles();
  this.color = color;
  for(var i = 0; i < cs.length; i++){
    cs[i].setColor(color);
  }
  this.canvas.renderAll();
}

Editor.prototype.showPreview = function(b64){
  var _this = this;
  var toRemove = this.preview;
  fabric.util.loadImage(b64, function(imageElement){
    var image = new fabric.Image(imageElement,{
      left: 0,
      top:  0,
      selectable: false
    });
    _this.preview = image;
    _this.canvas.add(image);
    _this.canvas.remove(toRemove)
    if(_this.onShowPreview){_this.onShowPreview(_this)}
  })
}

Editor.prototype.endPreview = function(){
  this.canvas.remove(this.preview);
  this.canvas.renderAll();
  this.preview = null;
  if(this.onEndPreview){this.onEndPreview(this)}
}
