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
    Rvs: TTakeProfitTiketBot;
    procedure BotOpenPosition(const ASander: TObject; AMode: TMode; APrice: Double);
    procedure BotClosePosition(const ASander: TObject; AMode: TMode; APrice: Double);
    procedure RvsOpenPosition(const ASander: TObject; AMode: TMode; APrice: Double);
    procedure RvsClosePosition(const ASander: TObject; AMode: TMode; APrice: Double);
  public
    BeginTime: Integer;
    SourceTikets: TSourceTikets;
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

  Rvs := TTakeProfitTiketBot.Create;
  Rvs.StopLoss := -10;
  Rvs.CountStop := 3;
  Rvs.TakeProfit := 50;
  Rvs.OnOpenPosition := RvsOpenPosition;
  Rvs.OnClosePosition := RvsClosePosition;


  SourceTikets := TSourceTikets.Create;
  BeginTime := 32400;
end;



procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(SourceTikets);
  FreeAndNil(Rvs);
  FreeAndNil(Bot);
end;

procedure TMainForm.ButtonReadClick(Sender: TObject);
var
  xFileName: String;
begin
  Memo1.Lines.Clear;
  Memo2.Lines.Clear;

  Bot.Default;
  Rvs.Default;

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
  case AMode of
    tmBuy: begin
      Memo1.Lines.Add('buy');
      Rvs.SetRvsMode(tmSell);
      Rvs.SetOpenPosition(APrice);
    end;
    tmSell: begin
      Memo1.Lines.Add('sell');
      Rvs.SetRvsMode(tmBuy);
      Rvs.SetOpenPosition(APrice);
    end;
  else
    Memo1.Lines.Add('error');
  end;
  Memo1.Lines.Add('open:' + APrice.ToString);
end;


procedure TMainForm.BotClosePosition(const ASander: TObject; AMode: TMode; APrice: Double);
begin
  Memo1.Lines.Add('bot ###');
  Memo1.Lines.Add('close:' + APrice.ToString);
  //*********************
  Series1.AddY(Bot.HealthPoints);
end;

procedure TMainForm.RvsOpenPosition(const ASander: TObject; AMode: TMode; APrice: Double);
begin
  Memo2.Lines.Add('rvs ###');
  case AMode of
    tmBuy: Memo2.Lines.Add('buy');
    tmSell: Memo2.Lines.Add('sell');
  else
    Memo2.Lines.Add('error');
  end;
  Memo2.Lines.Add('open:' + APrice.ToString);
end;

procedure TMainForm.RvsClosePosition(const ASander: TObject; AMode: TMode; APrice: Double);
begin
  Memo2.Lines.Add('rvs ###');
  Memo2.Lines.Add('close:' + APrice.ToString);
  LineSeries1.AddY(Rvs.HealthPoints);
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
        Rvs.SetUpPosition(xTiket.Last);
      end
      else
      begin
        if not Bot.IsPosition then
        begin
          if not Rvs.IsPosition then
          begin
            Bot.SetMode;
            Bot.SetOpenPosition(xTiket.Last);
          end;
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
