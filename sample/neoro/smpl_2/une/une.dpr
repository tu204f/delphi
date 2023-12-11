program une;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.Neuron in 'Lb.Neuron.pas',
  Lb.ActivationFunction in '..\..\tmp_smpl\lib\Lb.ActivationFunction.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
