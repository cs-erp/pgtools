# pgtools

Creative Solutions PostgreSQL backup tool

Portable backup tools with one click, to exports backup all databases you added to the list to local directory [expermode], in the same directory of application.

- Portable application, put it in your flash disk
- Backup and restore database with one click
- Restore database the last one, or select it by date
- Save point, backup your database to special name to restore it in future
- Rename password of pg user [expertmode]
- Backup all databases at one click, or restore it all [expertmode]
- Rename, Copy, Drop databases
- Clean all, it is special for Creative Solution, it is drop all databases with extension .old or .temp

You need to have PostgreSQL installed to you computer and added the directory of PG "bin" to the PATH in enviroments, or select the bin folder of PG in folders options.

Do not check "Creative Soultion products" to backup any databases, this check only for Creative Solutions databases.

If you need to enable Expert mode, open pgtools.ini, change expert to 1, expert=1

If you do not want it portable application, disable it by add option in pgtools.ini portable=0 , it save options in roaming folder instead of current folder

if you want it in arabic add Language=Arabic

For example if you want to distripute it with your application, add this ini to your pgtools.ini and distrupute it, this example with expert mode enabled

```ini
[Options]
Portable=0
Language=Arabic
Expert=1
```

## Compile

Use FPC 3.3 snapshot from date 2020-10

Lazarus from SVN, version 2.1 or later

