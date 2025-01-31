program test;

uses
  Vcl.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  UnitTreeListFrame in 'UnitTreeListFrame.pas' {TreeListFrame: TFrame},
  Lb.Logger in 'lib\Lb.Logger.pas',
  Lb.Setting in 'lib\Lb.Setting.pas',
  Lb.ApplicationVersion in 'lib\Lb.ApplicationVersion.pas',
  Lb.Message in 'lib\Lb.Message.pas',
  Lb.Test.SysUtils in 'lib\Lb.Test.SysUtils.pas';

{$R *.res}

begin
  TLogger.ClearLog;
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
