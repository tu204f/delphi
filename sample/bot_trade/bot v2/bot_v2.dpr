﻿program bot_v2;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.ABot in 'src\Lb.ABot.pas',
  Lb.Candel.Source in '..\src\Lb.Candel.Source.pas',
  Lb.Candel.SysUtils in '..\src\Lb.Candel.SysUtils.pas',
  Lb.Vecrot.Candel in 'src\Lb.Vecrot.Candel.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.