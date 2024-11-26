program pr_v2;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.ReadPrice in '..\lib\Lb.ReadPrice.pas',
  Lb.Pattern in 'Lb.Pattern.pas',
  UnitBarFrame in 'chart\UnitBarFrame.pas' {BarFrame: TFrame},
  UnitBarsFrame in 'chart\UnitBarsFrame.pas' {BarsFrame: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
