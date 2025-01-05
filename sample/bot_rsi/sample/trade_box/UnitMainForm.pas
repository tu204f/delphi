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
  Lb.TradeBox,
  Lb.Journal.Trading, System.Rtti, FMX.Grid.Style, FMX.Grid;

type
  TMainForm = class(TForm)
    Timer: TTimer;
    ButtonStartAndStop: TButton;
    Memo: TMemo;
    TextValue: TText;
    ButtonOpenShort: TButton;
    ButtonCloseShort: TButton;
    ButtonReverseShort: TButton;
    ButtonOpenLong: TButton;
    ButtonCloseLong: TButton;
    ButtonReversOpen: TButton;
    StrGrid: TStringGrid;
    procedure ButtonStartAndStopClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure ButtonOpenLongClick(Sender: TObject);
    procedure ButtonCloseLongClick(Sender: TObject);
    procedure ButtonOpenShortClick(Sender: TObject);
    procedure ButtonReverseShortClick(Sender: TObject);
    procedure ButtonCloseShortClick(Sender: TObject);
    procedure ButtonReversOpenClick(Sender: TObject);
  public const
    VALUE_MAX = 100;
    VALUE_MIN = 0;
  private
    FValue: Double;
    FTradeValue: Integer;
    function GetStepValue: Double;
  protected
    JournalTrading: TJournalTrading;
    procedure SetShowJournalTrading;
  protected
    TradeBox: TTradeBox;
    procedure EventTradeBox(ASender: TObject; ATypeDirection: TTypeDirection; ATypeTrade: TTypeTrade);
  protected
    procedure DoStart;
    procedure DoStop;
    procedure DoValue;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetLog(S: String);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);

  procedure _AddColumn(const AStrGrid: TStringGrid; const AHeader: String);
  var
    xCol: TStringColumn;
  begin
    xCol := TStringColumn.Create(nil);
    xCol.Header := AHeader;
    xCol.Parent := AStrGrid;
  end;

begin
  inherited;

  _AddColumn(StrGrid,'№');
  _AddColumn(StrGrid,'OpenTime');
  _AddColumn(StrGrid,'CloseTime');
  _AddColumn(StrGrid,'Side');
  _AddColumn(StrGrid,'OpenPrice');
  _AddColumn(StrGrid,'ClosePrice');
  _AddColumn(StrGrid,'MovingPrice');
  _AddColumn(StrGrid,'Qty');
  _AddColumn(StrGrid,'Sum.Value');
  _AddColumn(StrGrid,'IsActive');
  _AddColumn(StrGrid,'Profit');

  TradeBox := TTradeBox.Create;

  TradeBox.OnTradeBox := EventTradeBox;
  TradeBox.OpenLong := 50;
  TradeBox.CloseLong := 95;
  TradeBox.OpenShort := 50;
  TradeBox.CloseShort := 5;

  JournalTrading := TJournalTrading.Create;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(JournalTrading);
  FreeAndNil(TradeBox);
  inherited;
end;

procedure TMainForm.SetLog(S: String);
var
  xS: String;
begin
  xS := FormatDateTime('hh:mm:ss',Time) + ' :: ' + S;
  Memo.Lines.Add(xS);
end;

procedure TMainForm.ButtonStartAndStopClick(Sender: TObject);
begin
  if Timer.Enabled then
    DoStop
  else
    DoStart;
end;

procedure TMainForm.DoStart;
begin
  FValue := 0;
  FTradeValue := 0;
  ButtonStartAndStop.Text := 'Стоп';
  Timer.Enabled := True;
end;

procedure TMainForm.DoStop;
begin
  ButtonStartAndStop.Text := 'Старт';
  Timer.Enabled := False;
end;

function TMainForm.GetStepValue: Double;
begin
  Result := Random(5);
end;

procedure TMainForm.TimerTimer(Sender: TObject);
begin
  case FTradeValue of
    0: begin
      FValue := FValue + GetStepValue;
      if FValue >= VALUE_MAX then
      begin
        FTradeValue := 1;
        FValue := VALUE_MAX;
      end;
    end;
    1: begin
      FValue := FValue - GetStepValue;
      if FValue <= VALUE_MIN then
      begin
        FTradeValue := 0;
        FValue := VALUE_MIN;
      end;
    end;
  else
    DoStop;
  end;
  DoValue;
  SetShowJournalTrading;
end;

procedure TMainForm.DoValue;
var
  xS: String;
begin
  xS := 'Price(RSI): ' + FloatToStr(FValue);
  if JournalTrading.IsPosition then
    xS := xS + ' открыта';
  TextValue.Text := xS;
  TradeBox.SetUpDateValue(FValue);
end;

procedure TMainForm.EventTradeBox(ASender: TObject; ATypeDirection: TTypeDirection; ATypeTrade: TTypeTrade);

  procedure _TypeDirectionLong(ATypeTrade: TTypeTrade; AQty: Double; AAsk, ABid: Double);
  begin
    case ATypeTrade of
      ttOpen: begin
        if JournalTrading.IsPosition then
        begin
          if JournalTrading.CurrentPosition.Side = TTypeBuySell.tsSell then
          begin
            JournalTrading.ReverseTrade(
              GetNewDateTime,
              ABid
            );
          end;
        end
        else
        begin
          JournalTrading.OpenTrade(
            GetNewDateTime,
            AAsk,
            AQty,
            TTypeBuySell.tsBuy
          );
        end;
      end;
      ttClose: begin
        if JournalTrading.IsPosition then
          if JournalTrading.CurrentPosition.Side = TTypeBuySell.tsBuy then
          begin
            JournalTrading.CloseTrade(
              GetNewDateTime,
              ABid
            );
          end;
      end;
    end;
  end;

  procedure _TypeDirectionShort(ATypeTrade: TTypeTrade; AQty: Double; AAsk, ABid: Double);
  begin
    case ATypeTrade of
      ttOpen: begin
        if JournalTrading.IsPosition then
        begin
          if JournalTrading.CurrentPosition.Side = TTypeBuySell.tsBuy then
          begin
            JournalTrading.ReverseTrade(
              GetNewDateTime,
              AAsk
            );
          end;
        end
        else
        begin
          JournalTrading.OpenTrade(
            GetNewDateTime,
            ABid,
            AQty,
            TTypeBuySell.tsSell
          );
        end;
      end;
      ttClose: begin
        if JournalTrading.IsPosition then
          if JournalTrading.CurrentPosition.Side = TTypeBuySell.tsSell then
          begin
            JournalTrading.CloseTrade(
              GetNewDateTime,
              AAsk
            );
          end;
      end;
    end;
  end;

var
  xS: String;
begin
  xS := 'TradeBox: ' + GetStrToTypeDirection(ATypeDirection) + ' ' + GetStrToTypeTrade(ATypeTrade);
  SetLog(xS);
  if ATypeTrade = TTypeTrade.ttClose then
    SetLog('** *************');

  case ATypeDirection of
    tdLong: _TypeDirectionLong(ATypeTrade, 1, FValue + 1, FValue - 1);
    tdShort: _TypeDirectionShort(ATypeTrade, 1, FValue + 1, FValue - 1);
  end;
end;

procedure TMainForm.SetShowJournalTrading;
var
  i, iCount: Integer;
  xPosition: TJournalPosition;
begin
  // Ппоказывать проведенные торговые операции
  StrGrid.RowCount := 0;
  iCount := JournalTrading.Positions.Count;
  if iCount > 0 then
  begin
    StrGrid.RowCount := iCount;
    for i := 0 to iCount - 1 do
    begin
      xPosition := JournalTrading.Positions[i];

      StrGrid.Cells[0,i] := (i + 1).ToString;
      StrGrid.Cells[1,i] := xPosition.OpenTime.ToString;
      StrGrid.Cells[2,i] := xPosition.CloseTime.ToString;
      StrGrid.Cells[3,i] := GetStrToSide(xPosition.Side);
      StrGrid.Cells[4,i] := xPosition.OpenPrice.ToString;
      StrGrid.Cells[5,i] := xPosition.ClosePrice.ToString;
      StrGrid.Cells[6,i] := xPosition.MovingPrice.ToString;
      StrGrid.Cells[7,i] := xPosition.Qty.ToString;
      StrGrid.Cells[8,i] := xPosition.Value.ToString;

      if xPosition.IsActive then
        StrGrid.Cells[9,i] := 'Да'
      else
        StrGrid.Cells[9,i] := 'Нет';

      xPosition.SetProfit(FValue);
      StrGrid.Cells[10,i] := xPosition.Profit.ToString;

    end;
  end;
end;


procedure TMainForm.ButtonOpenLongClick(Sender: TObject);
var
  xCurrentDateTime: TDateTime;
begin
  // Открыть позицию
  xCurrentDateTime := GetNewDateTime;
  JournalTrading.OpenTrade(
    xCurrentDateTime,
    FValue,
    1,
    TTypeBuySell.tsBuy
  );
end;

procedure TMainForm.ButtonCloseLongClick(Sender: TObject);
var
  xCurrentDateTime: TDateTime;
begin
  // Закрыть позицию
  xCurrentDateTime := GetNewDateTime;
  JournalTrading.CloseTrade(
    xCurrentDateTime,
    FValue
  );
end;

procedure TMainForm.ButtonReversOpenClick(Sender: TObject);
var
  xCurrentDateTime: TDateTime;
begin
  // Реверс позиции
  xCurrentDateTime := GetNewDateTime;
  JournalTrading.ReverseTrade(
    xCurrentDateTime,
    FValue
  );
end;

procedure TMainForm.ButtonOpenShortClick(Sender: TObject);
var
  xCurrentDateTime: TDateTime;
begin
  // Открытие короткой позиции
  xCurrentDateTime := GetNewDateTime;
  JournalTrading.OpenTrade(
    xCurrentDateTime,
    FValue,
    1,
    TTypeBuySell.tsSell
  );
end;

procedure TMainForm.ButtonCloseShortClick(Sender: TObject);
var
  xCurrentDateTime: TDateTime;
begin
  // Закрытие позиции
  xCurrentDateTime := GetNewDateTime;
  JournalTrading.CloseTrade(
    xCurrentDateTime,
    FValue
  );
end;

procedure TMainForm.ButtonReverseShortClick(Sender: TObject);
var
  xCurrentDateTime: TDateTime;
begin
  // Реверсироваться позицию
  xCurrentDateTime := GetNewDateTime;
  JournalTrading.ReverseTrade(
    xCurrentDateTime,
    FValue
  );
end;



end.
