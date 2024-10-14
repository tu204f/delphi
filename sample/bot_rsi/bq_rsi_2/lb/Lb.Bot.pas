unit Lb.Bot;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  Lb.SysUtils;

type
  ///<summary>
  /// ��� - ��� ��������
  ///</summary>
  TBot = class(TObject)
  private
    FCountLoss: Integer;
    FProfitLoss: Double;
  protected
  public
    constructor Create; virtual;
    destructor Destroy; override;
    ///<summary>��������� ������</summary>
    procedure SetSelected;
    ///<summary>���������� ��������� ������</summary>
    property CountLoss: Integer read FCountLoss write FCountLoss;
    ///<summary>������ ������</summary>
    property ProfitLoss: Double read FProfitLoss write FProfitLoss;
  end;

implementation

{ TBot }

constructor TBot.Create;
begin
  FCountLoss := 10;

end;

destructor TBot.Destroy;
begin

  inherited;
end;

procedure TBot.SetSelected;
begin

end;

end.
