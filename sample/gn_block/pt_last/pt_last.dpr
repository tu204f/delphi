program pt_last;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.ReadPrice.Tiket in '..\lib\Lb.ReadPrice.Tiket.pas',
  Lb.Position.Trade in 'Lb.Position.Trade.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
