use strict;
use warnings;

use TreeOfTexts;

my $app = TreeOfTexts->apply_default_middlewares(TreeOfTexts->psgi_app);
$app;

