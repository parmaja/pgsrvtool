# PG Server GUI

GUI tool to run postgresql server in background, minimized to tray, without using the service, usefull for portable projects, or if you want postgresql on external harddisk with data, maybe encrypted harddisk.

You need to add pgserver.ini next the exe file

pgpath: Path to exe bin folder of postgresql
datapath=Path to data, it should be initialized by

    initdb.exe -W -U postgres -D "d:\data\pg13" -E UTF8

All paths are relative to app folder, you can point to your data folder like this '.\data\' , '..\data\' , '.\data\', '\data\' last one is point to root of same drive

tray: true if you want to make tray always show
minimized: start the application minimized
start: auto start server, work with minimzed too

Example of pgserver.ini

```
[options]
pgpath=\programs\pg13-64\bin
datapath=\data\pg13
tray=true
minimized=false
start=false
```

## Note

if you dont want to use this project, and want to install pg as server run this command once in admin privileges

    pg_ctl.exe register -N "pg13" -U "NT AUTHORITY\NetworkService" -D "d:\data\pg13" -w

## License

MIT(https://opensource.org/licenses/MIT)
