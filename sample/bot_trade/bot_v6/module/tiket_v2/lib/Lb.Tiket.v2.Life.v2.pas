unit Lb.Tiket.v2.Life.v2;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections;

type
  ///<summary>������� ��������� �������</summary>
  ///<remarks>�� ��������� ������ ������ ������� ������� ���� ��������� � ���� ���������</remarks>
  TTrader = class(TObject)
  private
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

{ TTrader }

constructor TTrader.Create;
begin

end;

destructor TTrader.Destroy;
begin

  inherited;
end;

end.
