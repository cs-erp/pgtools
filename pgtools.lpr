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

{procedure TranslateLCL;
var
  PODirectory, Lang, FallbackLang: String;
begin
  PODirectory := Application.Location;
  Lang:='en';
  FallbackLang:='em';
  LazGetLanguageIDs(Lang, FallbackLang); // in unit LazUTF8
  Translations.TranslateUnitResourceStrings('LCLStrConsts', PODirectory + 'pgtools.%s.po', Lang, FallbackLang);
end;}

begin
  //TranslateLCL;
  Application.Title :='PGTools';
  Application.Name := 'pgtools';
  RequireDerivedFormResource:= True;
  Application.Scaled :=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
