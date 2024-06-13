unit Lb.TradeBot;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  Lb.Bybit.SysUtils,
  Lb.Bybit.Kline;

type
  ///<summary>
  /// ������� �������
  ///</summary>
  TOnEventConditionTrade = procedure(ASender: TObject; ASide: TTypeSide) of object;
  ///<summary>
  /// ������ � ��������
  ///</summary>
  ///<remarks>
  /// TTypeCategory.tcLinear - �������� ������ �� �������� �������������
  ///</remarks>
  TConditionTrade = class(TObject)
  private
    FSide: TTypeSide;
    FIsActive: Boolean;
    FIsResuming: Boolean;
    FValueRSI: Double;
    FResumingRSI: Double;
    FEventConditionTrade: TOnEventConditionTrade;
  protected
    procedure DoEventConditionTrade;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    ///<summary>
    /// ���������� ��������
    ///</summary>
    procedure UpDataEvent(const ACurrentRSI: Double);
    ///<summary>
    /// ����������� ������
    ///</summary>
    property Side: TTypeSide read FSide write FSide;
    ///<summary>
    /// ��������� �������� ����������
    ///</summary>
    property ValueRSI: Double read FValueRSI write FValueRSI;
    ///<summary>
    /// �������� RSI - ��� �������������
    ///</summary>
    property ResumingRSI: Double read FResumingRSI write FResumingRSI;
    ///<summary>
    /// ���������� ������� ������
    ///</summary>
    property IsActive: Boolean read FIsActive write FIsActive;
    ///<summary>
    /// �������������
    ///</summary>
    property IsResuming: Boolean read FIsResuming write FIsResuming;
  end;


implementation

uses
  Lb.Setting,
  Lb.OperationTrade,
  Lb.Bybit.Trade;

{ TConditionTrade }

constructor TConditionTrade.Create;
begin
  FSide := TTypeSide.tsBuy;
  FIsActive := False;
  FIsResuming := False;
end;

destructor TConditionTrade.Destroy;
begin

  inherited;
end;

procedure TConditionTrade.DoEventConditionTrade;
begin
  if Assigned(FEventConditionTrade) then
    FEventConditionTrade(Self,FSide);
end;

procedure TConditionTrade.UpDataEvent(const ACurrentRSI: Double);
begin
  case FSide of
    tsBuy: begin
      if FIsActive then
      begin
        if FValueRSI > ACurrentRSI then
        begin
          DoEventConditionTrade;
          FIsActive := False;
        end;
      end
      else
      begin
        if FIsResuming then
          if FResumingRSI < ACurrentRSI then
            FIsActive := True;
      end;
    end;
    tsSell: begin
      if FIsActive then
      begin
        if FValueRSI < ACurrentRSI then
        begin
          DoEventConditionTrade;
          FIsActive := False;
        end;
      end
      else
      begin
        if FIsResuming then
          if FResumingRSI > ACurrentRSI then
            FIsActive := True;
      end;
    end;
  end;
end;

end.
