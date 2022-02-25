% top.pl

% toploop in xtop.pl

% main: entry point ??
% main:-toptest,fail.
% main:-toploop,fail;halt.


:-['lib.pl'].
:-['lists.pl'].
:-['dcg.pl'].
:-['shared.pl'].
:-['cserver.pl'].

:-['engine.pl'].
:-['meta.pl'].
:-['bagof.pl'].
:-['sort.pl'].

:-['parser.pl'].
:-['writer.pl'].

:-['xlib.pl'].
:-['xtop.pl'].
:-['xio.pl'].
:-['xbuiltins.pl'].
:-['jlib.pl'].

:-['db.pl']. % new indexed db

:-['xrefl.pl']. % general reflection

:-['xnet.pl']. % networking
:-['xtask.pl']. % multitasking, threads

:-['xbs.pl'].


% compiler
:-['../compiler/leanco.pl'].

% tests - moved to bin/progs
% :-['xtests.pl'].

% unessential

:-['edb.pl'].
% :-['ystate.pl'].
% :-['ycoop.pl'].

/*
:-['jtests.pl'].
:-['../bin/progs/nrev.pl'].
:-['../bin/progs/pISO.pro'].
:-['../bin/progs/pBDD.pro'].

:-['../bin/progs/bits.pro'].
:-['../bin/progs/dsyn.pro'].
*/



