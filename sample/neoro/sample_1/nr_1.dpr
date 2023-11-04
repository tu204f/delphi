program nr_1;

uses
  Vcl.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.NeuronNet in '..\lib\Lb.NeuronNet.pas',
  Lb.GeneticAlgorithm in '..\lib\Lb.GeneticAlgorithm.pas',
  Lb.ActivationFunction in '..\lib\Lb.ActivationFunction.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
