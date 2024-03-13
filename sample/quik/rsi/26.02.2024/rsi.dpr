program rsi;

uses
  Vcl.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Quik.Manager.DDE in '..\..\..\..\library\trade\quik\Quik.Manager.DDE.pas',
  Quik.SysUtils in '..\..\..\..\library\trade\quik\Quik.SysUtils.pas',
  Quik.ValueTable in '..\..\..\..\library\trade\quik\Quik.ValueTable.pas',
  Lb.Logger in '..\..\..\..\library\Lb.Logger.pas',
  UnitToolsForm in 'UnitToolsForm.pas' {ToolsForm},
  BTMemoryModule in '..\..\..\..\library\trade\libquik\BTMemoryModule.pas',
  QuikTrans2Order in '..\..\..\..\library\trade\libquik\QuikTrans2Order.pas',
  QuikTrans2QuikAPI in '..\..\..\..\library\trade\libquik\QuikTrans2QuikAPI.pas',
  QuikTransOrder in '..\..\..\..\library\trade\libquik\QuikTransOrder.pas',
  Lb.Setting in '..\..\..\..\library\Lb.Setting.pas',
  UnitQuikTableForm in 'UnitQuikTableForm.pas' {QuikTableForm},
  UnitAddOrderForm in 'UnitAddOrderForm.pas' {AddOrderForm},
  Lb.SysUtils in 'Lb.SysUtils.pas',
  UnitUserOrderFrame in 'UnitUserOrderFrame.pas' {UserOrderFrame: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TToolsForm, ToolsForm);
  Application.CreateForm(TQuikTableForm, QuikTableForm);
  Application.CreateForm(TAddOrderForm, AddOrderForm);
  Application.Run;
end.
