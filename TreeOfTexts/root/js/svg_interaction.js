var colors = ['#ffeeaa','#afc6e9','#d5fff6','#ffccaa','#ffaaaa','#e5ff80','#e5d5ff','#ffd5e5'];

$(document).ready(function() {
  $('svg').width('485px');
})

function de_color_nodes() {
    $('ellipse').attr( {stroke:'black', fill:'#fff'} );
    $('.node').children('polygon').attr( {stroke:'#fff', fill:'#fff'} );
    $('.node').children('text').attr( {stroke:'none', fill:'#000'} );
}

function do_color_nodes( topic, data, subscriberData ) {
 // data = {column_index, arr_node_ids, arr_greynode_ids}
    jQuery.each( data.arr_greynode_ids, function(index,value) {
      nodes = $('.node').children('title').filter( function(index) {
        return $(this).text() == value;
      })
      nodes.siblings('ellipse, polygon, text').each( function( index ) {
          $(this).attr( {stroke:'#ddd', fill:'#f8f8f8'} );
        });
    });
    jQuery.each( data.arr_node_ids, function(index,value) {
      $('.node').children('title').filter( function(index) {
        return $(this).text() == value;
      }).siblings('ellipse').each( function( index ) {
          $(this).attr( {stroke:'black', fill:colors[data.column_index-1]} );
        });
    });
}

function load_stemma_svg( topic, data, subscriberData ) {
    var params = {};
	var postData = {};

	postData.textid = data;

	params[gadgets.io.RequestParameters.METHOD] = gadgets.io.MethodType.POST;
	params[gadgets.io.RequestParameters.POST_DATA] = gadgets.io.encodeValues(postData);
	var url = "http://localhost:3000/svg_service";

	gadgets.io.makeRequest(url,
		function (o) {
			displaySVG(o.text);
		}, params);
	
}

function loaded() {
	gadgets.window.adjustHeight(400);
	subId = gadgets.Hub.subscribe("interedition.svg.dot.coloring", do_color_nodes);
	subId = gadgets.Hub.subscribe("interedition.svg.dot.decoloring", de_color_nodes);
	subId = gadgets.Hub.subscribe("interedition.tradition.selected", load_stemma_svg);
}

if (gadgets.util.hasFeature('pubsub-2')) {
	gadgets.HubSettings.onConnect = function(hum, suc, err) { loaded(); };
}
else gadgets.util.registerOnLoadHandler(loaded);
