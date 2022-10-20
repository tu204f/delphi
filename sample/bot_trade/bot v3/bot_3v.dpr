program bot_3v;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.Candel.Source in '..\src\Lb.Candel.Source.pas',
  Lb.SysUtils.Candel in '..\src\Lb.SysUtils.Candel.pas',
  Lb.Vecrot.Candel in '..\bot v2\src\Lb.Vecrot.Candel.pas',
  Lb.ChartsFrame in 'src\Lb.ChartsFrame.pas' {CandelsCharFrame: TFrame},
  Lb.Black.Box in 'src\Lb.Black.Box.pas',
  Lb.CandelFrame in 'src\Lb.CandelFrame.pas' {CandelFrame: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
