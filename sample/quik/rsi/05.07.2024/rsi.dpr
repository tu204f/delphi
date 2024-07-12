program rsi;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  UnitOrderCategoryFrame in 'UnitOrderCategoryFrame.pas' {OrderCategoryFrame: TFrame},
  UnitQuikTableForm in 'UnitQuikTableForm.pas' {QuikTableForm},
  Quik.Manager.DDE in '..\..\..\..\library\trade\quik\Quik.Manager.DDE.pas',
  Quik.SysUtils in '..\..\..\..\library\trade\quik\Quik.SysUtils.pas',
  Quik.ValueTable in '..\..\..\..\library\trade\quik\Quik.ValueTable.pas',
  Lb.Logger in '..\..\..\..\library\Lb.Logger.pas',
  Lb.Setting in '..\..\..\..\library\Lb.Setting.pas',
  UnitMainClientFrame in 'UnitMainClientFrame.pas' {MainClientFrame: TFrame},
  UnitCategoryFrame in 'UnitCategoryFrame.pas' {CategoryFrame: TFrame},
  Lb.SysUtils in 'Lb.SysUtils.pas';

{$R *.res}

begin
{$IFDEF DEBUG}
  TLogger.ClearLog;
{$ENDIF}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TQuikTableForm, QuikTableForm);
  Application.Run;
end.
