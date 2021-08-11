program vectors;



uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.Candel.Source in '..\candels\lib\Lb.Candel.Source.pas',
  Lb.Candel.SysUtils in '..\candels\lib\Lb.Candel.SysUtils.pas',
  Lb.Vector.Math in '..\candels\lib\Lb.Vector.Math.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
