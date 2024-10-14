program rsi_bq_2;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.Bot in 'lb\Lb.Bot.pas',
  Lb.SysUtils in 'lb\Lb.SysUtils.pas',
  Lb.Platfom in 'lb\Lb.Platfom.pas',
  BTMemoryModule in '..\..\..\library\trade\libquik\BTMemoryModule.pas',
  QuikTrans2Order in '..\..\..\library\trade\libquik\QuikTrans2Order.pas',
  QuikTrans2QuikAPI in '..\..\..\library\trade\libquik\QuikTrans2QuikAPI.pas',
  QuikTransOrder in '..\..\..\library\trade\libquik\QuikTransOrder.pas',
  Quik.Manager.DDE in '..\..\..\library\trade\quik\Quik.Manager.DDE.pas',
  Quik.SysUtils in '..\..\..\library\trade\quik\Quik.SysUtils.pas',
  Quik.ValueTable in '..\..\..\library\trade\quik\Quik.ValueTable.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
