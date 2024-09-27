program Training;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.NeuronNet.Files in 'Lb.NeuronNet.Files.pas',
  Lb.NeuronNet.Neuron in 'Lb.NeuronNet.Neuron.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
