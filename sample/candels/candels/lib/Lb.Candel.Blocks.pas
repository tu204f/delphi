unit Lb.Candel.Blocks;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  Lb.Candel.SysUtils,
  Lb.Candel.Source;

type
  ///<summary>Ѕлок Ц который отвечает за данные дл€ вектора</summary>
  TBlock = class(TObject)
  private
    FSourceCandel: TSourceCandel;
  public
    constructor Create;
    destructor Destroy; override;
    ///</summary>»сточник данных дл€ блока</summary>
    {todo: »сспользование объектов}
    property SourceCandel: TSourceCandel read FSourceCandel;
  end;

implementation

{ TBlock }

constructor TBlock.Create;
begin
  FSourceCandel := TSourceCandel.Create;
end;

destructor TBlock.Destroy;
begin
  FreeAndNil(FSourceCandel);
  inherited;
end;

end.
