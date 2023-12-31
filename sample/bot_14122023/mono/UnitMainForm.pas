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
  FMX.Dialogs,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Memo.Types,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.Objects,
  Lb.SysUtils,
  Lb.ReadPrice,
  Lb.Bot, FMXTee.Engine, FMXTee.Series, FMXTee.Procs, FMXTee.Chart;

type
  TMainForm = class(TForm)
    ButtonRead: TButton;
    TextStatus: TText;
    Memo1: TMemo;
    Chart1: TChart;
    Series1: TLineSeries;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonReadClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    SourceCandels: TSourceCandels;
    Bot: TTakeProfit;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Bot := TTakeProfit.Create;
  Bot.StopLoss := -10;
  Bot.CountStop := 5;
  Bot.TakeProfit := 20;

  SourceCandels := TSourceCandels.Create;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(SourceCandels);
  FreeAndNil(Bot);
end;

procedure TMainForm.ButtonReadClick(Sender: TObject);

  procedure _WorkBot;
  var
    xS: String;
    xCandel: TCandel;
    i, iCount: Integer;
    xMode: TMode;
  begin
    Bot.Clear;
    Series1.Clear;
    Memo1.Lines.Clear;
    iCount := SourceCandels.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xCandel := SourceCandels.Candels[i];
        xMode := Bot.Mode;
        Bot.SetCurrentCandel(xCandel);
        if xMode <> tmNull then
        begin
          Series1.AddY(Bot.HealthPoints);
          xS := Bot.HealthPoints.ToString;
          Memo1.Lines.Add(xS);
        end;
      end;
  end;

var
  xFileName: String;
begin

  xFileName := 'd:\work\git\delphi\sample\bot_14122023\data\SRZ3202211_13122023_14122023(1).txt';
  SourceCandels.LoadFromFile(xFileName);
  SourceCandels.Delete(0);
  TextStatus.Text := 'Количество строк:' + SourceCandels.Count.ToString;
  _WorkBot;
end;


end.
