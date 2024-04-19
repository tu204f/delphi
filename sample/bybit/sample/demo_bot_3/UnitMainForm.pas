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
  FMX.Layouts,
  FMX.Controls.Presentation,
  FMX.Memo.Types,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.Edit,
  FMX.ComboEdit,
  FMX.StdCtrls,
  Lb.Bybit.SysUtils,
  Lb.HistoryIndicator,
  Lb.Bybit.InstrumentsInfo,
  Lb.Bybit.OrderBook,
  UnitIndicatorFrame,
  UnitBotFrame,
  Lb.OperationTrade,
  FMX.Objects,
  FMX.TabControl,
  UnitSecurityFrame;

type
  TMainForm = class(TForm)
    LayoutMenu: TLayout;
    Layout: TLayout;
    ButtonStart: TButton;
    Timer: TTimer;
    ComboEditSymbol: TComboEdit;
    ButtonInstrumentsInfo: TButton;
    TabControl: TTabControl;
    TabItemTrade: TTabItem;
    TabItemSecurity: TTabItem;
    procedure ButtonStartClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FValueRSI: Double;
    FOperation: TTypeOperation;
  private
    ///<summary>
    /// Количесво
    ///</summary>
    FQuantity: Double;
    FEventCount: Integer;
    HistoryIndicator: THistoryIndicator;
    InstrumentPrice: TInstrumentPrice;
    procedure SetStart;
    procedure SetStop;
    procedure SetOperationTrade;
    procedure EventResponse(ASander: TObject; ATypeObject: TTypeObject);
    procedure SetLog(S: String);
  protected
    SecurityFrame: TSecurityFrame;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  System.TypInfo,
  System.RTTI,
  Lb.Params,
  Lb.Setting;

var
  ParamInstrumentInfo: TStringParams = nil;

{ TForm5 }

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FQuantity := 0.01;
  FOperation := TTypeOperation.toNull;

  // Загужаем история
  HistoryIndicator := THistoryIndicator.Create;
  HistoryIndicator.OnResponse := EventResponse;
  // Котеровальный стакан
  InstrumentPrice := TInstrumentPrice.Create;
  InstrumentPrice.OnResponse := EventResponse;

  // Параметры работы
  ParamInstrumentInfo := TStringParams.Create;

  // Таблица финасовых инструментов
  SecurityFrame := TSecurityFrame.Create(nil);
  SecurityFrame.Parent := TabItemSecurity;
  SecurityFrame.Align := TAlignLayout.Client;
end;


destructor TMainForm.Destroy;
begin
  FreeAndNil(SecurityFrame);
  FreeAndNil(ParamInstrumentInfo);
  FreeAndNil(InstrumentPrice);
  inherited;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  ParamInstrumentInfo.Load('instrument');

  // ОПеределяем параметры
  {todo: параметры перевести под настройку проекта}
  HistoryIndicator.Symbol := ParamInstrumentInfo.AsString['symbol'];
  HistoryIndicator.Category := TTypeCategory.tcLinear;
  HistoryIndicator.Interval := TTypeInterval.ti_1;
  HistoryIndicator.Period := 14;

  InstrumentPrice.Symbol := ParamInstrumentInfo.AsString['symbol'];
  InstrumentPrice.Category := TTypeCategory.tcLinear;
  InstrumentPrice.Limit := 10;

  ComboEditSymbol.Text := ParamInstrumentInfo.AsString['symbol'];
end;

procedure TMainForm.SetStart;
begin
  SetInitialization('','');

  ButtonStart.Text := 'Стоп';
  HistoryIndicator.UpDate;
  InstrumentPrice.UpDate;
  Timer.Enabled := True;
end;

procedure TMainForm.SetStop;
begin
  ButtonStart.Text := 'Старт';
  Timer.Enabled := False;
end;

procedure TMainForm.ButtonStartClick(Sender: TObject);
begin
  if Timer.Enabled then
    SetStop
  else
    SetStart;
end;

procedure TMainForm.TimerTimer(Sender: TObject);
begin
  FValueRSI := 0;
  FEventCount := 0;

  HistoryIndicator.UpDate;
  InstrumentPrice.UpDate;
end;

procedure TMainForm.SetLog(S: String);
//var
//  xS: String;
begin
//  xS := FormatDateTime('hh:mm:ss.zzz',Time) + ' || ' + S;
//  SetLogText(xS);
end;

procedure TMainForm.EventResponse(ASander: TObject; ATypeObject: TTypeObject);
begin
  Inc(FEventCount);
  case ATypeObject of
    tobNullObject: begin
      SetLog('MainForm.EventResponse.NullObject [' + FEventCount.ToString + ']');
    end;
    tobHistoryIndicator: begin
      FValueRSI := HistoryIndicator.RSI.Current.AvgValue;
    end;
    tobInstrumentPrice: begin
    end;
  end;
  if FEventCount = 2 then
    SetOperationTrade;
end;

procedure TMainForm.SetOperationTrade;
begin

end;

end.
