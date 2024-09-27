program pr_v1;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.ReadPrice in '..\lib\Lb.ReadPrice.pas',
  Lb.Trade.Data in 'Lb.Trade.Data.pas',
  Lb.Trade.Param in 'Lb.Trade.Param.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
