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
  FMX.Dialogs, FMX.Memo.Types, FMX.StdCtrls, FMX.Objects, FMXTee.Engine,
  FMXTee.Series, FMXTee.Procs, FMXTee.Chart, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo,
  Lb.SysUtils,
  Lb.ReadPrice,
  Lb.Bot.Tiket,
  FMX.Layouts;

type
  TMainForm = class(TForm)
    Memo1: TMemo;
    Chart1: TChart;
    Series1: TLineSeries;
    TextStatus: TText;
    ButtonRead: TButton;
    TimerRead: TTimer;
    Chart2: TChart;
    LineSeries1: TLineSeries;
    GridPanelLayout1: TGridPanelLayout;
    Memo2: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TimerReadTimer(Sender: TObject);
    procedure ButtonReadClick(Sender: TObject);
  private
    IndexTiket: Integer;
  protected
    Bot: TTakeProfitTiketBot;
    procedure BotOpenPosition(const ASander: TObject; AMode: TMode; APrice: Double);
    procedure BotClosePosition(const ASander: TObject; AMode: TMode; APrice: Double);
  public
    BeginTime: Integer;
    SourceTikets: TSourceTikets;
    BybitSourceTikets: TBybitSourceTikets;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Bot := TTakeProfitTiketBot.Create;
  Bot.StopLoss := -10;
  Bot.CountStop := 3;
  Bot.TakeProfit := 50;
  Bot.OnOpenPosition := BotOpenPosition;
  Bot.OnClosePosition := BotClosePosition;

  SourceTikets := TSourceTikets.Create;
  BeginTime := 32400;
end;



procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(SourceTikets);
  FreeAndNil(Bot);
end;

procedure TMainForm.ButtonReadClick(Sender: TObject);
var
  xFileName: String;
begin
  Memo1.Lines.Clear;
  Memo2.Lines.Clear;

  Bot.Default;

  Series1.Clear;
  LineSeries1.Clear;

  xFileName := 'd:\work\git\delphi\sample\bot_14122023\data\SRZ3202211_13122023_13122023.txt';
  SourceTikets.LoadFromFile(xFileName);
  SourceTikets.Delete(0);
  TextStatus.Text := 'Количество строк:' + SourceTikets.Count.ToString;

  BeginTime := 32400;
  IndexTiket := 0;
  TimerRead.Enabled := not TimerRead.Enabled;
end;

procedure TMainForm.BotOpenPosition(const ASander: TObject; AMode: TMode; APrice: Double);
begin
  Memo1.Lines.Add('bot ###');
  Memo1.Lines.Add('open:' + APrice.ToString);
end;


procedure TMainForm.BotClosePosition(const ASander: TObject; AMode: TMode; APrice: Double);
begin
  Memo1.Lines.Add('bot ###');
  Memo1.Lines.Add('close:' + APrice.ToString);
  //*********************
  Series1.AddY(Bot.HealthPoints);
end;

procedure TMainForm.TimerReadTimer(Sender: TObject);

  procedure _Read;
  var
    xTiket: TTiket;
    xBeginTime, xEndTime: Integer;
  begin
    try
      xBeginTime := BeginTime;
      xEndTime   := xBeginTime + 300;

      xTiket := SourceTikets.Tikets[IndexTiket];
      if (xBeginTime >= xTiket.Time) and (xTiket.Time < xEndTime) then
      begin
        Bot.SetUpPosition(xTiket.Last);
      end
      else
      begin
        if not Bot.IsPosition then
        begin
          Bot.SetMode;
          Bot.SetOpenPosition(xTiket.Last);
        end;
        BeginTime  := xEndTime;
      end;

      Inc(IndexTiket);
      TextStatus.Text :=
        IndexTiket.ToString + ' :: ' +
        xTiket.Time.ToString;

      if SourceTikets.Count < IndexTiket then
        TimerRead.Enabled := False;

    except
      TimerRead.Enabled := False;
    end;
  end;

var
  i, iCount: Integer;
begin
  iCount := 100;
  for i := 0 to iCount - 1 do
  begin
    if TimerRead.Enabled then
      _Read
    else
      Break;
  end;
end;


end.
