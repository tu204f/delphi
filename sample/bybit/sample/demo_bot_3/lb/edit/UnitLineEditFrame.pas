unit UnitLineEditFrame;

interface

{$I demo_bot.inc}

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
  FMX.Objects,
  FMX.Controls.Presentation,
  UnitEditFrame,
  Lb.Bybit.SysUtils;

type
  TNotifyEventLine = procedure(Sender: TObject; ASide: TTypeSide; AQty: Double) of object;

  TLineEditFrame = class(TFrame)
    LayoutCheckBox: TLayout;
    GridLayout: TGridPanelLayout;
    LayoutActive: TLayout;
    LayoutReActive: TLayout;
    TextTitle: TText;
    GridPanel: TGridPanelLayout;
    CheckBoxActive: TCheckBox;
    CheckBoxReActive: TCheckBox;
    Rectangle: TRectangle;
    LayoutQty: TLayout;
  private
    ActiveValueFrame: TValueFrame;
    ReActiveValueFrame: TValueFrame;
    QtyValueFrame: TValueFrame;
    FOnOperationTrade: TNotifyEventLine;
  protected
    procedure DoOperationTrade(ASide: TTypeSide; AQty: Double);
  public
    NameSetting: String;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetSave;
    procedure SetLoad;
    procedure SetUpData(const ASide: TTypeSide; const AValueRSI: Double);
    property OnOperationTrade: TNotifyEventLine write FOnOperationTrade;
  end;

implementation

{$R *.fmx}

uses
  Lb.Logger,
  Lb.Setting;

{ TLineEditFrame }

constructor TLineEditFrame.Create(AOwner: TComponent);

  function _GetCreareValueFrame(const ALayout: TLayout): TValueFrame;
  var
    xValue: TValueFrame;
  begin
    xValue := TValueFrame.Create(nil);
    xValue.Parent := ALayout;
    xValue.Align := TAlignLayout.Client;
    xValue.MaxValue := 100;
    xValue.MinValue := 0;
    xValue.Step := 1;
    Result := xValue;
  end;

begin
  inherited Create(AOwner);
  ActiveValueFrame   := _GetCreareValueFrame(LayoutActive);
  ReActiveValueFrame := _GetCreareValueFrame(LayoutReActive);
  QtyValueFrame      := _GetCreareValueFrame(LayoutQty);

  with QtyValueFrame do
  begin
    MaxValue := 100;
    MinValue := 0.01;
    Step := 0.01;
  end;

end;

destructor TLineEditFrame.Destroy;
begin
  FreeAndNil(QtyValueFrame);
  FreeAndNil(ActiveValueFrame);
  FreeAndNil(ReActiveValueFrame);
  inherited;
end;

procedure TLineEditFrame.DoOperationTrade(ASide: TTypeSide; AQty: Double);
begin
  if Assigned(FOnOperationTrade) then
    FOnOperationTrade(Self, ASide, AQty);
end;

procedure TLineEditFrame.SetLoad;
begin
  ActiveValueFrame.Value := TSetting.ReadInteger('config.' + NameSetting + '.act_value',50);
  ReActiveValueFrame.Value := TSetting.ReadInteger('config.' + NameSetting + '.re_act_value',50);
  QtyValueFrame.Value := TSetting.ReadFloat('config.' + NameSetting + '.qty',0.01);
end;

procedure TLineEditFrame.SetSave;
begin
  TSetting.WriteFloat('config.' + NameSetting + '.act_value',ActiveValueFrame.Value);
  TSetting.WriteFloat('config.' + NameSetting + '.re_act_value',ReActiveValueFrame.Value);
  TSetting.WriteFloat('config.' + NameSetting + '.qty',QtyValueFrame.Value);
end;

procedure TLineEditFrame.SetUpData(const ASide: TTypeSide; const AValueRSI: Double);

  procedure _SetOrder(const AValueRSI: Double);
  begin
    // Условия активайкии
    if not CheckBoxActive.IsChecked then
      Exit;
    {$IFDEF DEBUG_TRADE}
    TLogger.LogTree(0,'TLineEditFrame.SetUpData._SetOrder');
    {$ENDIF}
    if (AValueRSI > 0) then
    begin
      case ASide of
        tsBuy:
          if AValueRSI < ActiveValueFrame.Value then
          begin
            DoOperationTrade(ASide,QtyValueFrame.Value);
            CheckBoxActive.IsChecked := False;
            {$IFDEF DEBUG_TRADE}
            TLogger.LogTreeText(0,'>> Усливие выпольнено');
            {$ENDIF}
          end;
        tsSell:
          if AValueRSI > ActiveValueFrame.Value then
          begin
            DoOperationTrade(ASide,QtyValueFrame.Value);
            CheckBoxActive.IsChecked := False;
            {$IFDEF DEBUG_TRADE}
            TLogger.LogTreeText(0,'>> Усливие выпольнено');
            {$ENDIF}
          end;
      end;
    end;
  end;

  procedure _SetActiveOrder(const AValueRSI: Double);
  begin
    if not CheckBoxReActive.IsChecked then
      Exit;
    {$IFDEF DEBUG_TRADE}
    TLogger.LogTree(0,'TLineEditFrame.SetUpData._SetActiveOrder');
    {$ENDIF}
    if (AValueRSI > 0) then
    begin
      case ASide of
        tsBuy: begin
          if AValueRSI > ReActiveValueFrame.Value then
          begin
            CheckBoxActive.IsChecked := True;
            {$IFDEF DEBUG_TRADE}
            TLogger.LogTreeText(0,'>> Усливие выпольнено');
            {$ENDIF}
          end;
        end;
        tsSell: begin
          if AValueRSI < ReActiveValueFrame.Value then
          begin
            CheckBoxActive.IsChecked := True;
            {$IFDEF DEBUG_TRADE}
            TLogger.LogTreeText(0,'>> Усливие выпольнено');
            {$ENDIF}
          end;
        end;
      end;
    end;
  end;

begin
  {$IFDEF DEBUG_TRADE}
  TLogger.LogTree(0,'TLineEditFrame.SetUpData:' + Self.ClassName);
  TLogger.LogTreeText(3,'SID:' + GetStrToTypeSide(ASide));
  TLogger.LogTreeText(3,'RSI:' + AValueRSI.ToString);
  {$ENDIF}
  _SetOrder(AValueRSI);
  _SetActiveOrder(AValueRSI);
end;

end.
