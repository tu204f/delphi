program quik_connect;

{$R 'script_qpl.res' '..\..\library\trade\script\script_qpl.rc'}

uses
  Vcl.Forms,
  UnitMainForm in 'UnitMainForm.pas' {ConnectQuikForm},
  Quik.Manager.DDE in '..\..\library\trade\quik\Quik.Manager.DDE.pas',
  Quik.SysUtils in '..\..\library\trade\quik\Quik.SysUtils.pas',
  Quik.ValueTable in '..\..\library\trade\quik\Quik.ValueTable.pas',
  Lb.Logger in '..\..\library\Lb.Logger.pas',
  Lb.Script.QPile in '..\..\library\trade\script\Lb.Script.QPile.pas',
  Lb.Operation.V1 in 'operation\Lb.Operation.V1.pas',
  Lb.SysUtils.Table in 'Lb.SysUtils.Table.pas',
  Lb.Level in 'Lb.Level.pas',
  Lb.Custom.Trade in 'operation\Lb.Custom.Trade.pas',
  BTMemoryModule in '..\..\library\trade\libquik\BTMemoryModule.pas',
  QuikTrans2Order in '..\..\library\trade\libquik\QuikTrans2Order.pas',
  QuikTrans2QuikAPI in '..\..\library\trade\libquik\QuikTrans2QuikAPI.pas',
  QuikTransOrder in '..\..\library\trade\libquik\QuikTransOrder.pas',
  UnitSourceForm in 'UnitSourceForm.pas' {SourceForm},
  Lb.SysUtils.CandeTable in 'Lb.SysUtils.CandeTable.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TConnectQuikForm, ConnectQuikForm);
  Application.Run;
end.
