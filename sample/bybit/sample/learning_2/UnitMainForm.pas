unit UnitMainForm;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo;

type
  TMainForm = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  Lb.Sources;

{$R *.fmx}

procedure TMainForm.Button1Click(Sender: TObject);
var
  xSources: TSources;
  xCandel: TCandel;
begin
  Memo1.BeginUpdate;
  try
    xSources := TSources.Create;
    try
      xSources.Save('candel.csv');
      for xCandel in xSources do
      begin
        with Memo1.Lines do
        begin
          Add('***************************************************************');
          Add(' >> ID: ' + xCandel.ID.ToString);
          Add(' >> Дата и Время: ' + TimeToStr(xCandel.DateTime));
          Add(' >> Open: ' + xCandel.Open.ToString);
          Add(' >> High: ' + xCandel.High.ToString);
          Add(' >> Low: ' + xCandel.Low.ToString);
          Add(' >> Close: ' + xCandel.Close.ToString);
          Add(' >> Vol: ' + xCandel.Vol.ToString);
          Add(' >> ValueRIS: ' + xCandel.ValueRSI.ToString);
          Add(' >> AvgValueRSI: ' + xCandel.AvgValueRSI.ToString);
          Add(' >> Momentum: ' + xCandel.Momentum.ToString);
        end;
      end;
    finally
      FreeAndNil(xSources);
    end;
  finally
    Memo1.EndUpdate;
  end;
end;

end.
