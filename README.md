This is a stripped down and heavily modified fork of Eonblast's [emysql][emysql]
repository.  You should use Eonblast's version.

Of primary note is that prepared statement support is removed.  Outside of that,
the general code base should be simpler and cleaner.  However the underlying
design and architecture of this application is generally flawed.  This is just
our attempt to take what works for us and remove everything else to reduce the
LoC to maintain.  If you are looking for a modern erlang mysql driver with a
promising future, take a look at extend's [bank][bank] and
[bank_mysql][bank_mysql] projects.

Our motivation in publishing this is simply to share back our modifications.
We do not expect this to be useful to anyone else, nor is it supported.

This project is used and tested with Erlang R14B04 and MySQL 5.5.30

[emysql]: http://github.com/Eonblast/emysql
[bank]: http://github.com/extend/bank
[bank_mysql]: http://github.com/extend/bank_mysql
