program win_trade;

uses
  Vcl.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  BTMemoryModule in '..\..\..\..\library\trade\libquik\BTMemoryModule.pas',
  QuikTrans2Order in '..\..\..\..\library\trade\libquik\QuikTrans2Order.pas',
  QuikTrans2QuikAPI in '..\..\..\..\library\trade\libquik\QuikTrans2QuikAPI.pas',
  QuikTransOrder in '..\..\..\..\library\trade\libquik\QuikTransOrder.pas',
  Lb.Logger in '..\..\..\..\library\Lb.Logger.pas',
  Lb.Setting in '..\..\..\..\library\Lb.Setting.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
