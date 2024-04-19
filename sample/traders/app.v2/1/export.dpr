program export;

uses
  Vcl.Forms,
  System.SysUtils,
  System.Variants,
  System.Classes,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.Export in 'lb\Lb.Export.pas';

{$R *.res}

begin
  FormatSettings.DecimalSeparator := '.';
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
