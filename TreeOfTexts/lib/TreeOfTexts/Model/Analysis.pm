package TreeOfTexts::Model::Analysis;

use strict;
use warnings;

use base 'Catalyst::Model::Adaptor';

__PACKAGE__->config( 
	class => 'Text::Tradition::Analysis',
	args => { 'file' => TreeOfTexts->path_to( 't', 'data', 'florilegium.xml' ),
			  'stemmadot' => TreeOfTexts->path_to( 't', 'data', 'stemma_a.dot' ) },
 );

1;