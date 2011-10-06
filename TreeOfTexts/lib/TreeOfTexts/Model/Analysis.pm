package TreeOfTexts::Model::Analysis;

use strict;
use warnings;

use base 'Catalyst::Model::Adaptor';

__PACKAGE__->config( 
	class => 'Text::Tradition::Analysis',
	args => { 'traditions' => [
	          { 'file' => TreeOfTexts->path_to( 't', 'data', 'florilegium.xml' ),
			  'stemmadot' => TreeOfTexts->path_to( 't', 'data', 'stemma_a.dot' ) },
			  { 'file' => TreeOfTexts->path_to( 't', 'data', 'heinrichi.xml' ),
			  'stemmadot' => TreeOfTexts->path_to( 't', 'data', 'stemma_h.dot' ) },
			  { 'file' => TreeOfTexts->path_to( 't', 'data', 'parzival.xml' ),
			  'stemmadot' => TreeOfTexts->path_to( 't', 'data', 'stemma_p.dot' ) },
			  ] },
 );

1;