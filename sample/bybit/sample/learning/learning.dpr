program learning;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.Source in 'lib\Lb.Source.pas',
  Lb.NeuronNet.Neuron in 'lib\Lb.NeuronNet.Neuron.pas',
  Lb.NeuronNet.Files in 'lib\Lb.NeuronNet.Files.pas',
  Lb.Logger in '..\..\..\..\library\Lb.Logger.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
