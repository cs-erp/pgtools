program pgtools;
{**
 *  This file is part of the "Creative Solutions PGTools http://www.cserp.org/"
 *
 * @license   mit(https://opensource.org/licenses/MIT)
 *
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, MainForms;

{$R *.res}

begin
  Application.Title :='PGTools';
  RequireDerivedFormResource:= True;
  Application.Scaled :=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.