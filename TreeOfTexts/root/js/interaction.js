var colors = ['#ffeeaa','#afc6e9','#d5fff6','#e5d5ff','#ffaaaa','#e5ff80','#ffccaa','#ffd5e5'];
var row_triggered = false;
$(document).ready(function() {
  $('.rowid').click( function() {
    row_triggered = true;
    $('ellipse').attr( {stroke:'black', fill:'#fff'} );
    $(this).parent().nextAll('.clickable').children('span').click();
    row_triggered = false;
  });
})
function color_nodes( column_index, arr_node_ids, arr_greynode_ids ) {
  if( !row_triggered ) { 
    $('ellipse').attr( {stroke:'black', fill:'#fff'} ) 
  }; 
  jQuery.each( arr_greynode_ids, function(index,value) {
    $('.node').children('title').filter( function(index) {
      return $(this).text() == value;
    }).siblings('ellipse').each( function( index ) {
        $(this).attr( {stroke:'black', fill:'#eee'} );
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