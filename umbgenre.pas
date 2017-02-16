unit UMBGenre;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, db;

type
  TMBGenre = Class
    private
           FGenreName : String;
           FGenreDescription : String;
           FGenreID : Integer;
           FGenresCompositionsID : array of Integer;
           FNewGenre : Boolean;
           procedure SetGenreName( Name : String );
           procedure SetGenreDescription( Descr : String );
           procedure GetGenresCompositions(const SQLQuery :  TSQLQuery);
           function GetGenresCompositionID( Index : Integer ) : Integer;

    published

    public
           constructor Create(); overload;
           constructor Create(Name : String; Desc : String; const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction); overload;
           constructor Create(ID : Integer; const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction); overload;
           property GenreName : String read FGenreName write SetGenreName;
           property GenreDescription : String read FGenreDescription write SetGenreDescription;
           property GenreID : Integer read FGenreID;
           property GenresComposition[Index : Integer]: Integer read GetGenresCompositionID;
           procedure UpdateGenre(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
  end;

implementation

constructor TMBGenre.Create();
begin
     FGenreName := '';
     FGenreDescription := '';
     FGenreID := 0;
     FNewGenre := True;
end;

constructor TMBGenre.Create(Name : String; Desc : String; const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
begin
     //ДК, проверяем если добавлен новый редактор
     SQLQuery.Close;
     SQLQuery.SQL.Text:='SELECT id, description FROM genres where name=:bName'; //ДК, может LIMIT 1 воткнуть?
     SQLQuery.Params.ParamByName('bName').AsString:=Name;
     SQLQuery.Open;

     if SQLQuery.RecordCount > 0 then
     begin
          FGenreName:=Name;
          FGenreDescription:=SQLQuery.FieldByName('desc').AsString;
          FGenreID:=SQLQuery.FieldByName('id').AsInteger;
          FNewGenre := False;
     end
     else
     begin
          FGenreName := Name;
          FGenreDescription := Desc;
          FGenreID := 0;
          FNewGenre := True;
          UpdateGenre(SQLQuery, SQLTransaction);
     end;

end;

constructor TMBGenre.Create(ID : Integer; const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
begin
     SQLQuery.Close;
     SQLQuery.SQL.Text:='SELECT name, description FROM genres where id=:bID';
     SQLQuery.Params.ParamByName('bID').AsString:=IntToStr(ID);
     SQLQuery.Open;
     //ДК, нужна проверка что запрос вернул не пустой результат
     FGenreName:=SQLQuery.FieldByName('name').AsString;
     FGenreDescription:=SQLQuery.FieldByName('description').AsString;
     FGenreID:=ID;
     SQLQuery.Close;
     FNewGenre := False;
end;

procedure TMBGenre.UpdateGenre(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
begin
  if FNewGenre = True then
      begin
           SQLQuery.Close;
           SQLQuery.SQL.Text:='insert into genres (name, description) values (:bGenreName, :bGenreDescription )';
           SQLQuery.Params.ParamByName('bGenreName').AsString:=FGenreName;
           SQLQuery.Params.ParamByName('bGenreDescription').AsString:=FGenreDescription;
           SQLQuery.ExecSQL;
           SQLTransaction.Commit;
           FNewGenre := False;
      end
      else
      begin
          SQLQuery.Close;
          SQLQuery.SQL.Text:='update genres set name=:bGenreName, description=:bGenreDescription where id =:bGenreID';
          SQLQuery.Params.ParamByName('bGenreName').AsString:=FGenreName;
          SQLQuery.Params.ParamByName('bGenreDescription').AsString:=FGenreDescription;
          SQLQuery.Params.ParamByName('bGenreID').AsString:=IntToStr(FGenreID);
          SQLQuery.ExecSQL;
          SQLTransaction.Commit;
      end;
           SQLQuery.Close;
end;

procedure TMBGenre.SetGenreName( Name : String );
begin
  FGenreName := Name;
end;

procedure TMBGenre.SetGenreDescription(Descr : String);
begin
  FGenreDescription := Descr;
end;

procedure TMBGenre.GetGenresCompositions(const SQLQuery :  TSQLQuery);
var
  I : Integer;
begin
     SQLQuery.Close;
     SQLQuery.SQL.Text:='SELECT composition_id FROM rel_composition_genress where genre_id=:bID';
     SQLQuery.Params.ParamByName('bID').AsString:=IntToStr(FGenreID);
     SQLQuery.Open;
     SQLQuery.Last;
     if SQLQuery.RecordCount > 0 then
     begin
          SetLength(FGenresCompositionsID, SQLQuery.RecordCount);
          SQLQuery.First;
          I:=0;
          while not SQLQuery.EOF do
          begin
               FGenresCompositionsID[I]:=SQLQuery.FieldByName('composition_id').AsInteger;
               I:=I+1;
               SQLQuery.Next;
          end;
          SQLQuery.Close;
     end;
end;

function TMBGenre.GetGenresCompositionID( Index : Integer ) : Integer;
begin
     if Index < Length(FGenresCompositionsID) then
        Result := FGenresCompositionsID[Index];
end;

end.

