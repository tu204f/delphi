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
  Lb.ReadPrice,
  Lb.Trade,
  FMX.Layouts,
  FMX.ListBox,
  FMX.Objects, System.Rtti, FMX.Grid.Style, FMX.ScrollBox, FMX.Grid,
  FMX.TreeView;

type
  TMainForm = class(TForm)
    ButtonReadPrice: TButton;
    Timer: TTimer;
    ListBox: TListBox;
    Text1: TText;
    StrGrid: TStringGrid;
    procedure ButtonReadPriceClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    ValueRSI: Double;
    CurrentDate: TDateTime;
    CandelIndex: Integer;
    CandelsSource: TCandelsSource;
    Candels: TCandelList;
  private
    FCurrentTrade: TTrade;
    FDateTrade: TDateTrade;
    FDateTrades: TDateTradeList;
  protected
    procedure SetPriceLog(const S: String);
    procedure SetStartCandel(ACandel: TCandel);
    procedure SetUpDateCandel(ACandel: TCandel);
    procedure SetAddCandel(ACandel: TCandel);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetShowGrid;
    procedure SetSaveFile;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

function GetIndexToStr(const ACandelIndex: Integer): String;
var
  xS: String;
  xL: Integer;
begin
  xS := ACandelIndex.ToString;
  xL := xS.Length;
  case xL of
    1: Result := '000' + xS;
    2: Result := '00' + xS;
    3: Result := '0' + xS;
  else
    Result := xS;
  end;
end;

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);

  procedure _AddCol(AHeader: String; AGrid: TStringGrid);
  var
    xCol: TStringColumn;
  begin
    xCol := TStringColumn.Create(nil);
    xCol.Header := AHeader;
    xCol.Parent := AGrid;
  end;

begin
  inherited Create(AOwner);

  _AddCol('Date', StrGrid);
  _AddCol('Count', StrGrid);
  _AddCol('Profit', StrGrid);
  _AddCol('CountTakeProfit', StrGrid);
  _AddCol('CountStopLoss', StrGrid);
  _AddCol('CountNull', StrGrid);

  FCurrentTrade := nil;
  CandelsSource := TCandelsSource.Create;
  Candels := TCandelList.Create;

  FDateTrade  := nil;
  FDateTrades := TDateTradeList.Create;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(Candels);
  FreeAndNil(CandelsSource);
  FreeAndNil(FDateTrades);
  inherited;
end;


procedure TMainForm.ButtonReadPriceClick(Sender: TObject);
var
  xFileName: String;
begin
  Timer.Enabled := not Timer.Enabled;
  if Timer.Enabled then
  begin
    CandelIndex := 1;
    ButtonReadPrice.Text := 'Стоп';
    xFileName := ExtractFilePath(ParamStr(0)) + 'data\';
    xFileName := xFileName + 'GZU4.csv';
    CandelsSource.LoadFromFile(xFileName);
    ListBox.Items.Clear;
  end
  else
    ButtonReadPrice.Text := 'Старт';
end;

procedure TMainForm.TimerTimer(Sender: TObject);
var
  xCandel: TCandel;
begin
  try
    xCandel := CandelsSource.Candels[CandelIndex];
    if CandelIndex = 1 then
    begin
      SetStartCandel(xCandel);
      CurrentDate := xCandel.Date;
    end
    else
    begin
      if CurrentDate = xCandel.Date then
        SetUpDateCandel(xCandel)
      else
      begin
        SetStartCandel(xCandel);
        CurrentDate := xCandel.Date;
      end;
    end;
    Inc(CandelIndex);
    if CandelIndex >= CandelsSource.Count then
    begin
      Timer.Enabled := False;
      ButtonReadPrice.Text := 'Старт';
      SetSaveFile;
    end;
  except
    Timer.Enabled := False;
    ButtonReadPrice.Text := 'Старт';
    SetSaveFile;
  end;
end;


procedure TMainForm.SetPriceLog(const S: String);
begin
  ListBox.Items.Add(S);
  if ListBox.Items.Count > 20 then
    ListBox.Items.Delete(0);
end;


procedure TMainForm.SetStartCandel(ACandel: TCandel);
var
  xS: String;
begin
  FDateTrade := TDateTrade.Create(ACandel.Date);
  FDateTrades.Add(FDateTrade);


  SetPriceLog('start');
  xS :=
    '[' + GetIndexToStr(CandelIndex) + '] ' +
    DateTimeToStr(ACandel.Date + ACandel.Time) + ' ' +
    ACandel.Close.ToString + ' ' +
    ACandel.Vol.ToString;

  SetPriceLog(xS);
  SetAddCandel(ACandel);
end;

procedure TMainForm.SetUpDateCandel(ACandel: TCandel);
var
  xS: String;
begin
  xS :=
    '[' + GetIndexToStr(CandelIndex) + '] ' +
    DateTimeToStr(ACandel.Date + ACandel.Time) + ' ' +
    ACandel.Close.ToString + ' ' +
    ACandel.Vol.ToString;

  SetPriceLog(xS);
  SetAddCandel(ACandel);

end;

procedure TMainForm.SetAddCandel(ACandel: TCandel);
const
  SIZE_RSI = 50;
var
  xInputParam: TInputParam;
begin
  if not Assigned(FDateTrade) then
    Exit;

  Candels.Add(ACandel);
  if Candels.Count > SIZE_RSI then
    Candels.Delete(0);

  ValueRSI := GetRSI(Candels);
  Text1.Text := 'Оценка состояние рынка: ' + ValueRSI.ToString;

  if Candels.Count = SIZE_RSI then
  begin

    xInputParam.Date := ACandel.Date;
    xInputParam.Time := ACandel.Time;
    xInputParam.Price := ACandel.Close;

    if ValueRSI > 50 then
    begin
      xInputParam.BuySell := 'B';
      xInputParam.ValueRSI := ValueRSI;
      // Покупаем
      if not Assigned(FCurrentTrade) then
      begin
        FCurrentTrade := TTrade.Create(xInputParam);
        FDateTrade.Trades.Add(FCurrentTrade);
      end
      else if FCurrentTrade.BuySell = 'S' then
      begin
        FCurrentTrade := TTrade.Create(xInputParam);
        FDateTrade.Trades.Add(FCurrentTrade);
      end;
    end
    else
    begin
      xInputParam.BuySell := 'S';
      xInputParam.ValueRSI := ValueRSI;
      // Продаем
      if not Assigned(FCurrentTrade) then
      begin
        FCurrentTrade := TTrade.Create(xInputParam);
        FDateTrade.Trades.Add(FCurrentTrade);
      end
      else if FCurrentTrade.BuySell = 'B' then
      begin
        FCurrentTrade := TTrade.Create(xInputParam);
        FDateTrade.Trades.Add(FCurrentTrade);
      end;
    end;
    if Assigned(FCurrentTrade) then
      FCurrentTrade.SetUpDate(ACandel.High,ACandel.Low,ACandel.Close);

    if Assigned(FDateTrade) then
      FDateTrade.SetUpDateTrade;

    SetShowGrid;
  end;
end;

procedure TMainForm.SetSaveFile;
var
  xStr: TStrings;
  xTrade: TTrade;
  xDateTrade: TDateTrade;
  i, iCount: Integer;
  j, jCount: Integer;
begin
  xStr := TStringList.Create;
  try
    jCount := FDateTrades.Count;
    if jCount > 0 then
      for j := 0 to jCount - 1 do
      begin
        xDateTrade := FDateTrades.Items[j];
        xStr.Add('new_date_' + DateToStr(xDateTrade.Date));
        iCount := xDateTrade.Trades.Count;
        if iCount > 0 then
          for i := 0 to iCount - 1 do
          begin
            xTrade := xDateTrade.Trades[i];
            xStr.Add(xTrade.ToString);
          end;
      end;
    xStr.SaveToFile('data.csv');
  finally
    FreeAndNil(xStr);
  end;
end;

procedure TMainForm.SetShowGrid;
var
  xDateTrade: TDateTrade;
  i, iCount: Integer;
begin
  iCount := FDateTrades.Count;
  StrGrid.RowCount := iCount;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xDateTrade := FDateTrades[i];
      StrGrid.Cells[0,i] := DateToStr(xDateTrade.Date);
      StrGrid.Cells[1,i] := xDateTrade.Trades.Count.ToString;
      StrGrid.Cells[2,i] := xDateTrade.Profit.ToString;
      StrGrid.Cells[3,i] := xDateTrade.CountTakeProfit.ToString;
      StrGrid.Cells[4,i] := xDateTrade.CountStopLoss.ToString;
      StrGrid.Cells[5,i] := xDateTrade.CountNull.ToString;
    end;
end;


end.
