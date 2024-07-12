unit UnitStatusFrame;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Objects,
  FMX.Controls.Presentation,
  FMX.Edit,
  Lb.SysUtils,
  Lb.Bybit.SysUtils,
  Lb.HistoryIndicator,
  Lb.Bybit.Position;

type
  ///<summary>
  /// Статус информации
  ///</summary>
  ///<remarks>
  /// Здесь управления — осуществляется чтение
  ///</remarks>
  TStatusFrame = class(TFrame)
    Rectangle1: TRectangle;
    EditQty: TEdit;
    EditValueRSI: TEdit;
    Timer: TTimer;
    procedure TimerTimer(Sender: TObject);
  private
    FHistoryIndicator: THistoryIndicator;
    FInstrumentPrice: TInstrumentPrice;
    FBybitPosition: TBybitPosition;
    procedure EventResponse(ASander: TObject; ATypeObject: TTypeObject);
    procedure EventBybitPositionEndLoading(Sender: TObject);
    function GetIsActive: Boolean;
  protected
    property HistoryIndicator: THistoryIndicator read FHistoryIndicator;
    property InstrumentPrice: TInstrumentPrice read FInstrumentPrice;
  public
    ValueRSI: Double;
    Bid, Ask: Double;
    Qty: Double;
    Side: TTypeSide;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    property IsActive: Boolean read GetIsActive;
  end;

implementation

{$R *.fmx}

{ TStatusFrame }

constructor TStatusFrame.Create(AOwner: TComponent);
begin
  inherited;
  FHistoryIndicator := THistoryIndicator.Create;
  FHistoryIndicator.OnResponse := EventResponse;

  FInstrumentPrice := TInstrumentPrice.Create;
  FInstrumentPrice.OnResponse := EventResponse;

  FBybitPosition := TBybitPosition.Create;
  FBybitPosition.OnEventEndLoading := EventBybitPositionEndLoading;
end;

destructor TStatusFrame.Destroy;
begin
  FreeAndNil(FBybitPosition);
  FreeAndNil(FInstrumentPrice);
  FreeAndNil(FHistoryIndicator);
  inherited;
end;

procedure TStatusFrame.Start;
begin
  if not Timer.Enabled then
  begin
    Timer.Enabled := True;

    HistoryIndicator.Symbol   := ParamApplication.Symble;
    HistoryIndicator.Category := ParamApplication.Category;
    HistoryIndicator.Interval := ParamApplication.Interval;
    HistoryIndicator.UpDate;

    InstrumentPrice.Symbol   := ParamApplication.Symble;
    InstrumentPrice.Category := ParamApplication.Category;
    InstrumentPrice.Limit    := 10;

    FBybitPosition.Symbol := ParamApplication.Symble;
    FBybitPosition.Category := ParamApplication.Category;
    FBybitPosition.SetEncryption(
      ParamApplication.ApiKey,
      ParamApplication.ApiSecret
    );
  end;
end;

procedure TStatusFrame.Stop;
begin
  if Timer.Enabled then
    Timer.Enabled := False;
end;


procedure TStatusFrame.TimerTimer(Sender: TObject);
begin
  HistoryIndicator.UpDate;
  InstrumentPrice.UpDate;
  FBybitPosition.Selected;
end;

procedure TStatusFrame.EventBybitPositionEndLoading(Sender: TObject);
var
  xF: TFormatSettings;
begin
  Qty := 0;
  if FBybitPosition.PositionObjects.Count > 0 then
  begin
    xF := FormatSettings;
    xF.DecimalSeparator := '.';
    Qty := StrToFloatDef(FBybitPosition.PositionObjects[0].Size,0,xF);
    Side := GetTypeSideToStr(FBybitPosition.PositionObjects[0].Side);
  end;
  EditQty.Text := '(' + GetStrToTypeSide(Side) + ')' + FloatToStr(Qty);
end;

procedure TStatusFrame.EventResponse(ASander: TObject; ATypeObject: TTypeObject);
begin
  case ATypeObject of
    tobHistoryIndicator: begin
      ValueRSI := HistoryIndicator.RSI.Current.AvgValue;
    end;
    tobInstrumentPrice: begin
      Bid := InstrumentPrice.Bid;
      Ask := InstrumentPrice.Ask;
    end;
  end;

  EditValueRSI.Text := 'RSI:' + FloatToStr(ValueRSI) +
  ' Price: ' + FloatToStr(Bid) + '/' + FloatToStr(Ask);
end;

function TStatusFrame.GetIsActive: Boolean;
begin
  Result := Timer.Enabled;
end;

end.
