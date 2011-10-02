$(document).ready(function() {
  $('.rowid').click( function() {
    $(this).parent().nextAll('.clickable').children('span').click();
  });
})
function color_nodes( arr_node_ids ) {
  $('ellipse').attr( {stroke:'black', fill:'#999'} );
  jQuery.each( arr_node_ids, function(index,value) {
    $('.node').children('title').filter( function(index) {
      return $(this).text() == value;
    }).siblings('ellipse').attr( {stroke:'red', fill:'yellow'} );;
  });
}
