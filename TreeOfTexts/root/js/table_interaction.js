var colors = ['#ffeeaa','#afc6e9','#d5fff6','#ffccaa','#ffaaaa','#e5ff80','#e5d5ff','#ffd5e5'];
var row_triggered = false;

function color_nodes( column_index, arr_node_ids, arr_greynode_ids ) {
  if( !row_triggered ) {
    $('tr.active_variant_row').children('td').removeClass('cellb0 cellb1 cellb2 cellb3 cellb4 cellb5 cellb6 cellb7'); 
    $('td.active_variant_cell').removeClass('active_variant_cell');
    de_color_nodes();
  }; 
  $('tr.active_variant_row').removeClass('active_variant_row') 
  do_color_nodes( column_index, arr_node_ids, arr_greynode_ids );
}

function de_color_nodes() {
    gadgets.Hub.publish("interedition.svg.dot.decoloring");
}

function do_color_nodes( column_index, arr_node_ids, arr_greynode_ids ) {
    data = {};
    data.column_index = column_index;
    data.arr_node_ids = arr_node_ids;
    data.arr_greynode_ids = arr_greynode_ids;
    gadgets.Hub.publish("interedition.svg.dot.coloring", data);
}

function load_tradition_table( topic, data, subscriberData ) {
    var params = {};
	var postData = {};

	postData.textid = data;

	params[gadgets.io.RequestParameters.METHOD] = gadgets.io.MethodType.POST;
	params[gadgets.io.RequestParameters.POST_DATA] = gadgets.io.encodeValues(postData);
	var url = "http://localhost:3000/table_service";

	gadgets.io.makeRequest(url,
		function (o) {
			$('#variants_table').html(o.text);
		}, params);
	
}

function loaded() {
	gadgets.window.adjustHeight(400);
	subId = gadgets.Hub.subscribe("interedition.tradition.selected", load_tradition_table);
}
if (gadgets.util.hasFeature('pubsub-2')) {
	gadgets.HubSettings.onConnect = function(hum, suc, err) { loaded(); };
}
else gadgets.util.registerOnLoadHandler(loaded);
