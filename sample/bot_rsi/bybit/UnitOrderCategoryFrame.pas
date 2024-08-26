unit UnitOrderCategoryFrame;

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
  FMX.Layouts,
  UnitCategoryFrame,
  Lb.Bybit.SysUtils,
  Lb.SysUtils;

type
  ///<summary>Заявка</summary>
  TOrderCategoryFrame = class(TFrame)
    GridPanel: TGridPanelLayout;
    Layout1: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    Layout4: TLayout;
    Layout5: TLayout;
  private
    FOnEventSendTarde: TOnEventSendTarde;
    FSide: TTypeSide;
    procedure InitCategoryFrame;
    procedure SetSide(const Value: TTypeSide);
    procedure SetColor(const Value: TAlphaColor);
  protected
    Category1: TCategoryFrame;
    Category2: TCategoryFrame;
    Category3: TCategoryFrame;
    Category4: TCategoryFrame;
    Category5: TCategoryFrame;
    procedure EventSendTarde(Sender: TObject; ATradeParam: TTradeParam);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetParams(const AParam: TSituationParam);
    property Side: TTypeSide read FSide write SetSide;
    property OnEventSendTarde: TOnEventSendTarde write FOnEventSendTarde;
    property Color: TAlphaColor write SetColor;
  end;

implementation

{$R *.fmx}

{ TOrderCategoryFrame }

constructor TOrderCategoryFrame.Create(AOwner: TComponent);
begin
  inherited;
  InitCategoryFrame;
end;

destructor TOrderCategoryFrame.Destroy;
begin
  FreeAndNil(Category1);
  FreeAndNil(Category2);
  FreeAndNil(Category3);
  FreeAndNil(Category4);
  FreeAndNil(Category5);
  inherited;
end;


procedure TOrderCategoryFrame.InitCategoryFrame;

  function _InitCategoryFrame(const ALayout: TLayout): TCategoryFrame;
  var
    xFrame: TCategoryFrame;
  begin
    xFrame := TCategoryFrame.Create(nil);
    xFrame.Parent := ALayout;
    xFrame.Align := TAlignLayout.Client;
    xFrame.OnEventSendTarde := EventSendTarde;
    Result := xFrame;
  end;

begin
  Category1 := _InitCategoryFrame(Layout1);
  Category2 := _InitCategoryFrame(Layout2);
  Category3 := _InitCategoryFrame(Layout3);
  Category4 := _InitCategoryFrame(Layout4);
  Category5 := _InitCategoryFrame(Layout5);
end;

procedure TOrderCategoryFrame.SetSide(const Value: TTypeSide);

  function _CrossSide(ASide: TTypeSide): TTypeSide;
  begin
    if ASide = TTypeSide.tsBuy then
      Result := TTypeSide.tsSell
    else
      Result := TTypeSide.tsBuy;
  end;

  procedure _CategoryFrame(ACategory: TCategoryFrame; ATypeTrade: TTypeTrade;
    ATypeLine: TTypeLine; ASide: TTypeSide);
  begin
    ACategory.TypeTrade := ATypeTrade;
    ACategory.TypeLine  := ATypeLine;
    ACategory.Side      := ASide;
    ACategory.Load;
  end;

begin
  FSide := Value;
  case FSide of
    tsBuy: begin
      _CategoryFrame(Category1,TTypeTrade.toLong,TTypeLine.tlClose,_CrossSide(FSide));
      _CategoryFrame(Category2,TTypeTrade.toLong,TTypeLine.tlOpen4,FSide);
      _CategoryFrame(Category3,TTypeTrade.toLong,TTypeLine.tlOpen3,FSide);
      _CategoryFrame(Category4,TTypeTrade.toLong,TTypeLine.tlOpen2,FSide);
      _CategoryFrame(Category5,TTypeTrade.toLong,TTypeLine.tlOpen1,FSide);
    end;
    tsSell: begin
      _CategoryFrame(Category1,TTypeTrade.toShort,TTypeLine.tlOpen1,FSide);
      _CategoryFrame(Category2,TTypeTrade.toShort,TTypeLine.tlOpen2,FSide);
      _CategoryFrame(Category3,TTypeTrade.toShort,TTypeLine.tlOpen3,FSide);
      _CategoryFrame(Category4,TTypeTrade.toShort,TTypeLine.tlOpen4,FSide);
      _CategoryFrame(Category5,TTypeTrade.toShort,TTypeLine.tlClose,_CrossSide(FSide));
    end;
  end;

end;

procedure TOrderCategoryFrame.SetColor(const Value: TAlphaColor);
begin
  Category1.Color := Value;
  Category2.Color := Value;
  Category3.Color := Value;
  Category4.Color := Value;
  Category5.Color := Value;
end;

procedure TOrderCategoryFrame.SetParams(const AParam: TSituationParam);
begin
  Category1.SetValueParam(AParam);
  Category2.SetValueParam(AParam);
  Category3.SetValueParam(AParam);
  Category4.SetValueParam(AParam);
  Category5.SetValueParam(AParam);
end;


procedure TOrderCategoryFrame.EventSendTarde(Sender: TObject; ATradeParam: TTradeParam);
begin
  if Assigned(FOnEventSendTarde) then
    FOnEventSendTarde(Sender,ATradeParam);
end;

end.
