program nr_2;

uses
  Vcl.Forms,
  UnitMainFrom in 'UnitMainFrom.pas' {Form4},
  Lb.NeuronNet in '..\lib\Lb.NeuronNet.pas',
  Lb.GA in 'Lb.GA.pas',
  Lb.NeuronNet.V2 in '..\lib\Lb.NeuronNet.V2.pas',
  Lb.ActivationFunction in '..\lib\Lb.ActivationFunction.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
