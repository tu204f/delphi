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
  FMX.Memo.Types,
  FMX.Objects,
  FMX.Layouts,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.Edit,
  FMX.StdCtrls,
  Lb.ReadPrice,
  Lb.Trade.Data, System.Rtti, FMX.Grid.Style, FMX.Grid;

type
  TMainForm = class(TForm)
    ButtonStart: TButton;
    Timer: TTimer;
    ButtonStop: TButton;
    ProgressBar: TProgressBar;
    StrGrid: TStringGrid;
    Text1: TText;
    Text2: TText;
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  protected
    CandelIndex: Integer;
    CandelsSource: TCandelsSource;
    procedure SetLoadSource;
    procedure SetOperationTrade(AValueRSI: Double; ACandel: TCandel);
  public
    BuySell: Char;
    CurTrade: TTrade;
    Trades: TTradeList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetShowGrid;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

const
  PERIOD_RSI = 50;

{ TMainForm }

procedure TMainForm.ButtonStopClick(Sender: TObject);
begin
  if Timer.Enabled then
  begin
    Timer.Enabled := False;
    Trades.Delete(Trades.Count - 1);
    Trades.Delete(Trades.Count - 1);

    SetLoadSource;
  end;
end;

constructor TMainForm.Create(AOwner: TComponent);

  procedure _AddCol(AGrid: TStringGrid; AHeader: String);
  var
    xCol: TStringColumn;
  begin
    xCol := TStringColumn.Create(AGrid);
    xCol.Header := AHeader;
    xCol.Parent := AGrid;
  end;

begin
  inherited;

  _AddCol(StrGrid,'O.Date');
  _AddCol(StrGrid,'O.Time');
  _AddCol(StrGrid,'C.Date');
  _AddCol(StrGrid,'C.Time');
  _AddCol(StrGrid,'RSI');
  _AddCol(StrGrid,'Price');
  _AddCol(StrGrid,'Value');
  _AddCol(StrGrid,'BuySell');
  _AddCol(StrGrid,'ProfitFirst');
  _AddCol(StrGrid,'ProfitFirstMax');
  _AddCol(StrGrid,'ProfitFirstMin');
  _AddCol(StrGrid,'Profit');
  _AddCol(StrGrid,'ProfitMax');
  _AddCol(StrGrid,'ProfitMin');
  _AddCol(StrGrid,'IsFirst');

  CandelsSource := TCandelsSource.Create;
  Trades := TTradeList.Create;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(Trades);
  FreeAndNil(CandelsSource);
  inherited;
end;

procedure TMainForm.ButtonStartClick(Sender: TObject);
var
  xFileName: String;
begin
  if not Timer.Enabled then
  begin
    CurTrade := nil;
    Trades.Clear;

    BuySell := #0;
    Trades.Clear;

    CandelIndex := 0;
    xFileName := ExtractFilePath(ParamStr(0)) + 'data\';

    //xFileName := xFileName + 'GAZP_240401_240630.csv';
    //xFileName := xFileName + 'GAZP_240401_240630.csv';
    //xFileName := xFileName + 'GAZP_240501_240731.csv';
    xFileName := xFileName + 'GAZP_240601_240804.csv';
    //xFileName := xFileName + 'GAZP_240701_240917.csv';
    //xFileName := xFileName + 'SPFB.GAZR-9.24_240701_240919.csv';

    CandelsSource.LoadFromFile(xFileName);
    CandelsSource.Delete(0);
    Timer.Enabled := True;
  end;
end;


procedure TMainForm.SetOperationTrade(AValueRSI: Double; ACandel: TCandel);

  function GetBuySell(AValueRSI: Double): Char;
  begin
    Result := 'N';
    if AValueRSI > 50 then
      Result := 'S'
    else if AValueRSI < 50 then
      Result := 'B'
  end;

var
  xBuySell: Char;
begin
  if Assigned(CurTrade) then
  begin

    if CurTrade.IsFirst then
      CurTrade.SetUpData(ACandel)
    else
    begin
      CurTrade.SetUpDataFirst(ACandel);
      CurTrade.SetUpData(ACandel);
    end;

    if CurTrade.ProfitFirst < 0 then
    begin
      CurTrade.SetCloseDateTime(ACandel.Date,ACandel.Time);
      CurTrade := nil;
    end else if CurTrade.ProfitMax > 200 then
    begin
      CurTrade.SetCloseDateTime(ACandel.Date,ACandel.Time);
      CurTrade := nil;
    end
    else if CurTrade.ProfitMin < -100 then
    begin
      CurTrade.SetCloseDateTime(ACandel.Date,ACandel.Time);
      CurTrade := nil;
    end
    else if ACandel.Time > StrToTime('20:00:00') then
    begin
      CurTrade.SetCloseDateTime(ACandel.Date,ACandel.Time);
      CurTrade := nil;
    end;

  end
  else
  begin

    xBuySell := GetBuySell(AValueRSI);
    if not (xBuySell = BuySell) then
    begin
      if (ACandel.Time > StrToTime('10:00:00')) and  (ACandel.Time < StrToTime('18:00:00')) then
      begin
        if (not Assigned(CurTrade)) and CharInSet(xBuySell,['B','S']) then
        begin
          CurTrade := TTrade.Create;
          CurTrade.ValueRSI := AValueRSI;
          CurTrade.SetOpenDateTime(ACandel.Date,ACandel.Time);
          CurTrade.BuySell := xBuySell;
          CurTrade.Price := ACandel.Close;
          CurTrade.Qty   := 100;
          Trades.Add(CurTrade);
        end;
      end;
      BuySell := xBuySell;
    end;

  end;

  SetShowGrid;
end;

procedure TMainForm.TimerTimer(Sender: TObject);

  procedure _Timer;
  var
    xValueRSI: Double;
    xCandel: TCandel;
    i: Integer;
    xBufferCandels: TCandelList;
  begin
    try
      xBufferCandels := TCandelList.Create;
      try
        if CandelIndex < (CandelsSource.Count - PERIOD_RSI) then
        begin
          for i := 0 to PERIOD_RSI - 1 do
          begin
            xCandel := CandelsSource.Candels[CandelIndex + i];
            xBufferCandels.Add(xCandel);
          end;
        end
        else
          ButtonStopClick(nil);
        ProgressBar.Value := 100 * CandelIndex/(CandelsSource.Count - PERIOD_RSI);
        xValueRSI := GetRSI(xBufferCandels);

        if (xCandel.Date <> 0) then
          SetOperationTrade(xValueRSI,xCandel);

      finally
        FreeAndNil(xBufferCandels);
      end;
      Text1.Text := CandelIndex.ToString;
      Inc(CandelIndex);
    except
      ButtonStopClick(nil);
    end;
  end;

var
  i: Integer;
begin
  for i := 0 to 99 do
    _Timer;
end;

procedure TMainForm.SetShowGrid;
var
  xSum: Double;
  xTrade: TTrade;
  i, iCount, xInd: Integer;
begin
  xSum := 0;
  iCount := Trades.Count;
  StrGrid.RowCount := iCount;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xTrade := Trades[i];
      xInd := (iCount - 1) - i;
      xSum := xSum + xTrade.Profit;

      StrGrid.Cells[0,xInd]  := DateToStr(xTrade.OpenDate);
      StrGrid.Cells[1,xInd]  := TimeToStr(xTrade.OpenTime);


      if xTrade.CloseDate <> 0 then
      begin
        StrGrid.Cells[2,xInd]  := DateToStr(xTrade.CloseDate);
        StrGrid.Cells[3,xInd]  := TimeToStr(xTrade.CloseTime);
      end
      else
      begin
        StrGrid.Cells[2,xInd]  := '';
        StrGrid.Cells[3,xInd]  := '';
      end;

      StrGrid.Cells[4,xInd]  := xTrade.ValueRSI.ToString;
      StrGrid.Cells[5,xInd]  := xTrade.Price.ToString;
      StrGrid.Cells[6,xInd]  := xTrade.Value.ToString;
      StrGrid.Cells[7,xInd]  := xTrade.BuySell;
      StrGrid.Cells[8,xInd]  := xTrade.ProfitFirst.ToString;
      StrGrid.Cells[9,xInd]  := xTrade.ProfitFirstMax.ToString;
      StrGrid.Cells[10,xInd]  := xTrade.ProfitFirstMin.ToString;
      StrGrid.Cells[11,xInd]  := xTrade.Profit.ToString;
      StrGrid.Cells[12,xInd] := xTrade.ProfitMax.ToString;
      StrGrid.Cells[13,xInd] := xTrade.ProfitMin.ToString;
      StrGrid.Cells[14,xInd] := xTrade.IsFirst.ToString;
    end;
  Text2.Text := 'Профит:' + xSum.ToString;
end;

procedure TMainForm.SetLoadSource;
var
  xS: String;
  xStr: TStrings;
  xTrade: TTrade;
  i, iCount: Integer;
begin
  xStr := TStringList.Create;
  try
    iCount := Trades.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xTrade := Trades[i];

        xS := '';
        xS := xS + DateToStr(xTrade.OpenDate) + ';';
        xS := xS + TimeToStr(xTrade.OpenTime) + ';';
        xS := xS + DateToStr(xTrade.CloseDate) + ';';
        xS := xS + TimeToStr(xTrade.CloseTime) + ';';
        xS := xS + xTrade.ValueRSI.ToString + ';';
        xS := xS + xTrade.Price.ToString + ';';
        xS := xS + xTrade.Value.ToString + ';';
        xS := xS + xTrade.BuySell + ';';
        xS := xS + xTrade.ProfitFirst.ToString + ';';
        xS := xS + xTrade.ProfitFirstMax.ToString + ';';
        xS := xS + xTrade.ProfitFirstMin.ToString + ';';
        xS := xS + xTrade.Profit.ToString + ';';
        xS := xS + xTrade.ProfitMax.ToString + ';';
        xS := xS + xTrade.ProfitMin.ToString + ';';
        xS := xS + xTrade.IsFirst.ToString + ';';

        xStr.Add(xS);
      end;
    xStr.SaveToFile('data_result.csv');
  finally
    FreeAndNil(xStr);
  end;
end;


end.
