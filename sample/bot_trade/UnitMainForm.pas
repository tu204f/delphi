unit UnitMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Lb.Candel.SysUtils,
  Lb.Candel.Source, Vcl.Grids;

type
  TMainForm = class(TForm)
    Timer: TTimer;
    ButtonLoad: TButton;
    MemoLog: TMemo;
    ButtonStart: TButton;
    StrGrid: TStringGrid;
    CurrentStrGrid: TStringGrid;
    ButtonStream: TButton;
    procedure FormShow(Sender: TObject);
    procedure ButtonLoadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure ButtonStreamClick(Sender: TObject);
  private
    FIndexCandel: Integer;
    procedure _Start;
    procedure _Stop;
  protected
    procedure SetLog(S: String);
  public
    SourceCandel: TSourceCandel;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  BotTrade;

var
  localDefaultBot: TDefaultBot = nil;


{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  SourceCandel := TSourceCandel.Create;
  localDefaultBot := TDefaultBot.Create;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(localDefaultBot);
  FreeAndNil(SourceCandel);
end;

procedure TMainForm.FormShow(Sender: TObject);

  procedure _GridTitle(const AGrid: TStringGrid);
  begin
    with AGrid.Rows[0] do
    begin
      Clear;
      Add('OpenDate');
      Add('OpenTime');
      Add('CloseDate');
      Add('CloseTime');
      Add('Open');
      Add('Close');
      Add('Quantity');
      Add('Direction');
      Add('Status');
      Add('Profit');
    end;
  end;

begin
  Self.Caption := 'Работы объекта';

  _GridTitle(StrGrid);
  _GridTitle(CurrentStrGrid);

  localDefaultBot.Init(150,30);
end;

procedure TMainForm.SetLog(S: String);
var
  xS: String;
begin
  xS := FormatDateTime('hh:nn:ss.zzz',Time) + ' || ' + S;
  MemoLog.Lines.Add(xS);
end;

procedure TMainForm.ButtonLoadClick(Sender: TObject);
var
  xPath: String;
begin
  xPath := ExtractFilePath(ParamStr(0)) + 'source' + PathDelim + 'candel.csv';
  SourceCandel.SetLoadFile(xPath);
  SetLog('Количество свечей: Count: ' + IntToStr(SourceCandel.Candels.Count));
end;

procedure TMainForm._Start;
begin
  if SourceCandel.Candels.Count > 0 then
  begin
    FIndexCandel := 0;
    ButtonStart.Caption := 'Стоп';
    Timer.Enabled := True;
  end;
end;

procedure TMainForm._Stop;
begin
  ButtonStart.Caption := 'Старт';
  Timer.Enabled := False;
end;

procedure TMainForm.ButtonStartClick(Sender: TObject);
begin
  // Старт стоп жизни
  if Timer.Enabled then
    _Stop
  else
    _Start;
end;

procedure TMainForm.ButtonStreamClick(Sender: TObject);
var
  xS: String;
  xL: INteger;
  xCnadelStream: TCnadelStream;
begin
  xCnadelStream := TCnadelStream.Create;
  try
    xL := 100;
    xCnadelStream.FileName := 'd:\work\git\delphi\sample\data\sber\data.csv';
    xS := xCnadelStream.First.ToString;
    while not xCnadelStream.EOF do
    begin
      xS := xCnadelStream.Next.ToString;
      MemoLog.Lines.Add(xS);
      Dec(xL);
      if xL <= 0 then
        Break;
    end;
  finally
    FreeAndNil(xCnadelStream);
  end;
end;

procedure TMainForm.TimerTimer(Sender: TObject);

  procedure _CurrentShowGrid;
  var
    xTradePosition: TTradePosition;
  begin
    xTradePosition := localDefaultBot.Position;
      with CurrentStrGrid.Rows[1] do
      begin
        Clear;
        Add(DateToStr(xTradePosition.OpenDate));
        Add(TimeToStr(xTradePosition.OpenTime));
        Add(DateToStr(xTradePosition.CloseDate));
        Add(TimeToStr(xTradePosition.CloseTime));
        Add(FloatToStr(xTradePosition.Open));
        Add(FloatToStr(xTradePosition.Close));
        Add(IntToStr(xTradePosition.Quantity));
        Add(GetDirectionToStr(xTradePosition.Direction));
        Add(GetStatusToStr(xTradePosition.Status));
        Add(FloatToStr(xTradePosition.GetProfitPerUnit));
      end;
  end;

  procedure _ShowGrid;
  var
    xTradePosition: TTradePosition;
  begin
    var xInd := 1;
    StrGrid.RowCount := localDefaultBot.Positions.Count + 2;
    for xTradePosition in localDefaultBot.Positions do
    begin
      with StrGrid.Rows[xInd] do
      begin
        Clear;
        Add(DateToStr(xTradePosition.OpenDate));
        Add(TimeToStr(xTradePosition.OpenTime));
        Add(DateToStr(xTradePosition.CloseDate));
        Add(TimeToStr(xTradePosition.CloseTime));
        Add(FloatToStr(xTradePosition.Open));
        Add(FloatToStr(xTradePosition.Close));
        Add(IntToStr(xTradePosition.Quantity));
        Add(GetDirectionToStr(xTradePosition.Direction));
        Add(GetStatusToStr(xTradePosition.Status));
        Add(FloatToStr(xTradePosition.GetProfitPerUnit));
      end;
      Inc(xInd);
    end;
  end;


  procedure _NextCandel;
  begin
    Inc(FIndexCandel);
    if FIndexCandel >= SourceCandel.Candels.Count then
    begin
      localDefaultBot.ExecutionStop;
      _Stop;
      _ShowGrid;
      _CurrentShowGrid;
    end;
  end;


var
  xCandel: TCandel;
begin
  xCandel := SourceCandel.Candels[FIndexCandel];

  localDefaultBot.ExecutionLastCandel(xCandel);
  _ShowGrid;
  _CurrentShowGrid;

  _NextCandel;
end;

end.
