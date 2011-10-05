var colors = ['#ffeeaa','#afc6e9','#d5fff6','#ffccaa','#ffaaaa','#e5ff80','#e5d5ff','#ffd5e5'];
var row_triggered = false;
$(document).ready(function() {
  $('.rowid').click( function() {
    row_triggered = true;
    $('ellipse').attr( {stroke:'black', fill:'#fff'} );
    $('tr.active_variant_row').children('td').removeClass('cellb0 cellb1 cellb2 cellb3 cellb4 cellb5 cellb6 cellb7'); 
    $(this).parent().nextAll('.clickable').children('span').click();
    $('td.active_variant_cell').removeClass('active_variant_cell');
    row_triggered = false;
  });
})
function color_nodes( column_index, arr_node_ids, arr_greynode_ids ) {
  if( !row_triggered ) {
    $('tr.active_variant_row').children('td').removeClass('cellb0 cellb1 cellb2 cellb3 cellb4 cellb5 cellb6 cellb7'); 
    $('td.active_variant_cell').removeClass('active_variant_cell');
    $('ellipse').attr( {stroke:'black', fill:'#fff'} );
  }; 
  $('tr.active_variant_row').removeClass('active_variant_row') 
  jQuery.each( arr_greynode_ids, function(index,value) {
    $('.node').children('title').filter( function(index) {
      return $(this).text() == value;
    }).siblings('ellipse').each( function( index ) {
        $(this).attr( {stroke:'black', fill:'#444'} );
      });
  });
  jQuery.each( arr_node_ids, function(index,value) {
    $('.node').children('title').filter( function(index) {
      return $(this).text() == value;
    }).siblings('ellipse').each( function( index ) {
        $(this).attr( {stroke:'black', fill:colors[column_index-1]} );
      });
  });
}

// 8 columns tied to colors, based on td position I guess
// click on row id: all colors on, remaining (unused) nodes greyed
// click on cell: those colors on, unused nodes greyed
// c

// 