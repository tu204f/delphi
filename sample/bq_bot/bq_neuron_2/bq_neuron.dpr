program bq_neuron;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.NeuronNet.Neuron in 'lb\Lb.NeuronNet.Neuron.pas',
  Lb.NeuronNet.SysUtils in 'lb\Lb.NeuronNet.SysUtils.pas',
  Lb.NeuronNet.Files in 'lb\Lb.NeuronNet.Files.pas',
  Lb.Table.CSV in 'lb\Lb.Table.CSV.pas',
  Lb.SysUtils in '..\bq_rsi_2\lb\Lb.SysUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
