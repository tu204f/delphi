program table;

uses
  FastMM4,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Quik.Manager.DDE in '..\..\..\library\trade\quik\Quik.Manager.DDE.pas',
  Quik.SysUtils in '..\..\..\library\trade\quik\Quik.SysUtils.pas',
  Quik.ValueTable in '..\..\..\library\trade\quik\Quik.ValueTable.pas',
  Lb.Logger in '..\..\..\library\Lb.Logger.pas';

{$R *.res}

begin
  FormatSettings.DecimalSeparator := '.';
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  TLogger.ClearLog;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
