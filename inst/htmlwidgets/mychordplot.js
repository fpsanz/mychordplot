HTMLWidgets.widget({

  name: 'mychordplot',

  type: 'output',

  factory: function(el, width, height) {

    // TODO: define shared variables for this instance

    return {

      renderValue: function(x) {
am4core.useTheme(am4themes_animated);
// Themes end

var chart = am4core.create("chartdiv", am4charts.ChordDiagram);


chart.data = x.data;



chart.dataFields.fromName = "from";
chart.dataFields.toName = "to";
chart.dataFields.value = "value";

chart.nodePadding = 0.5;
chart.minNodeSize = 0.01;
chart.startAngle = 80;
chart.endAngle = chart.startAngle + 360;
chart.sortBy = "value";
chart.fontSize = 10;


// make nodes draggable
var nodeTemplate = chart.nodes.template;
nodeTemplate.readerTitle = "Click to show/hide or drag to rearrange";
nodeTemplate.showSystemTooltip = true;
nodeTemplate.propertyFields.fill = "color";
nodeTemplate.tooltipText = "{name}: {total}";

nodeTemplate.events.on("over", function(event) {
    var node = event.target;
    node.outgoingDataItems.each(function(dataItem) {
        if(dataItem.toNode){
            dataItem.link.isHover = true;
            dataItem.toNode.label.isHover = true;
        }
    });
    node.incomingDataItems.each(function(dataItem) {
        if(dataItem.fromNode){
            dataItem.link.isHover = true;
            dataItem.fromNode.label.isHover = true;
        }
    });

    node.label.isHover = true;
});

// when rolled out from the node, make all the links rolled-out
nodeTemplate.events.on("out", function(event) {
    var node = event.target;
    node.outgoingDataItems.each(function(dataItem) {
        if(dataItem.toNode){
            dataItem.link.isHover = false;
            dataItem.toNode.label.isHover = false;
        }
    });
    node.incomingDataItems.each(function(dataItem) {
        if(dataItem.fromNode){
            dataItem.link.isHover = false;
           dataItem.fromNode.label.isHover = false;
        }
    });

    node.label.isHover = false;
});

var label = nodeTemplate.label;
label.relativeRotation = 90;

label.fillOpacity = 0.4;
let labelHS = label.states.create("hover");
labelHS.properties.fillOpacity = 1;

chart.nodes.template.label.disabled = true;

nodeTemplate.cursorOverStyle = am4core.MouseCursorStyle.pointer;
// this adapter makes non-main character nodes to be filled with color of the main character which he/she kissed most
nodeTemplate.adapter.add("fill", function(fill, target) {
    let node = target;
    let counters = {};
    let mainChar = false;
    node.incomingDataItems.each(function(dataItem) {

        if(isNaN(counters[dataItem.fromName])){
            counters[dataItem.fromName] = dataItem.value;
        }
        else{
            counters[dataItem.fromName] += dataItem.value;
        }
    });
    if(mainChar){
        return fill;
    }

    let count = 0;
    let color;
    let biggest = 0;
    let biggestName;

    for(var name in counters){
        if(counters[name] > biggest){
            biggestName = name;
            biggest = counters[name];
        }
    }

    return fill;
});

var nodeLink = chart.links.template;
var bullet = nodeLink.bullets.push(new am4charts.CircleBullet());
bullet.fillOpacity = 1;
bullet.circle.radius = 3;
bullet.locationX = 0.5;

// create animations
chart.events.on("ready", function() {
    for (var i = 0; i < chart.links.length; i++) {
        var link = chart.links.getIndex(i);
        var bullet = link.bullets.getIndex(0);

        animateBullet(bullet);
    }
});

function animateBullet(bullet) {
    var duration = 3000 * Math.random() + 2000;
    var animation = bullet.animate([{ property: "locationX", from: 0, to: 1 }], duration);
    animation.events.on("animationended", function(event) {
        animateBullet(event.target.object);
    });
}


// Enable export
chart.exporting.menu = new am4core.ExportMenu();
chart.exporting.menu.align = "left";
chart.exporting.menu.verticalAlign = "top";

} // fin renderValue
    }; //fin return
  } //fin factory
}); //ifn HTMLwidget
