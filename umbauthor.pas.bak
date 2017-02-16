unit UMBAuthor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, db;

type
    TMBAuthor = Class
      private
             FAuthorName : String;
             FAuthorSurname : String;
             FAuthorID : Integer;
             FAuthorOriginID : Integer;
             FNewAuthor : Boolean;
             FAuthorCompositions : array of Integer;
             FAuthorBooks : array of Integer;
             function GetAuthorBookID(Index : Integer) : Integer;
             procedure GetAuthorBooks(const SQLQuery :  TSQLQuery);
             procedure GetAuthorCompositions(const SQLQuery :  TSQLQuery);
             procedure SetAuthorName(Name : String = '');
             procedure SetAuthorSurname(Surname : String ='');
             function GetAuthorCompositionID(Index : Integer) : Integer;
      published

      public
             property AuthorName : String read FAuthorName write SetAuthorName;
             property AuthorSurname : String read FAuthorSurname write SetAuthorSurname;
             property AuthorID : Integer read FAuthorID;
             property AuthorOrigID : Integer read FAuthorOriginID;
             property AuthorCompositions[Index : Integer]: Integer read GetAuthorCompositionID;
             property AuthorBooks[Index : Integer]: Integer read GetAuthorBookID;
             constructor Create(); overload;
             constructor Create(Name : String; Surname : String; const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction); overload;
             constructor Create(ID : Integer; const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction); overload;
             procedure UpdateAuthor(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
             procedure AddComposition(ID : Integer; const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
    end;

implementation

constructor TMBAuthor.Create();
begin
     FAuthorName:='';
     FAuthorSurname:='';
     FAuthorID:=0;
     FAuthorOriginID:=0;
     FNewAuthor := True;
end;

constructor TMBAuthor.Create(Name : String; Surname : String; const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
begin
     //ДК, проверяем если добавлен новый автор
     SQLQuery.Close;
     SQLQuery.SQL.Text:='SELECT id  FROM authors where name=:bName and surname=:bSurname'; //ДК, может LIMIT 1 воткнуть?
     SQLQuery.Params.ParamByName('bName').AsString:=Name;
     SQLQuery.Params.ParamByName('bSurname').AsString:=Surname;
     SQLQuery.Open;

     if SQLQuery.RecordCount > 0 then
     begin
          FAuthorName:=Name;
          FAuthorSurname:=Surname;
          FAuthorID:=SQLQuery.FieldByName('id').AsInteger;
          FNewAuthor := False;
     end
     else
     begin
         //ДК, надо добавить пару проверок Name и Surname не должны быть пустыми
         FAuthorName:=Name;
         FAuthorSurname:=Surname;
         FAuthorID:=0;
         FAuthorOriginID:=0;
         FNewAuthor := True;
         UpdateAuthor(SQLQuery, SQLTransaction);
     end;

end;

constructor TMBAuthor.Create(ID : Integer; const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
begin
     SQLQuery.Close;
     SQLQuery.SQL.Text:='SELECT name, surname, originid FROM authors where id=:bID';
     SQLQuery.Params.ParamByName('bID').AsString:=IntToStr(ID);
     SQLQuery.Open;
     //ДК, нужна проверка что запрос вернул не пустой результат
     FAuthorName:=SQLQuery.FieldByName('name').AsString;
     FAuthorSurname:=SQLQuery.FieldByName('surname').AsString;
     FAuthorID:=ID;
     FAuthorOriginID:=StrToInt(SQLQuery.FieldByName('originid').AsString);
     SQLQuery.Close;
     FNewAuthor := False;
end;

//private функция
//получает масив ID произведений автора и сохраняет его в поле FAuthorCompositions
procedure TMBAuthor.GetAuthorCompositions(const SQLQuery :  TSQLQuery);
var
   I : Integer;
begin
     SQLQuery.Close;
     SQLQuery.SQL.Text:='SELECT composition_id FROM rel_composition_authors where author_id=:bID';
     SQLQuery.Params.ParamByName('bID').AsString:=IntToStr(FAuthorID);
     SQLQuery.Open;
     SQLQuery.Last;

     if SQLQuery.RecordCount > 0 then
     begin
          SetLength(FAuthorCompositions, SQLQuery.RecordCount);
          SQLQuery.First;
          I:=0;
          while not SQLQuery.EOF do
          begin
               FAuthorCompositions[I]:=SQLQuery.FieldByName('composition_id').AsInteger;
               I:=I+1;
               SQLQuery.Next;
          end;
          SQLQuery.Close;
     end;
end;

              //private функция
              //получает массив ID книг в которые вошли произведения автора и
              //сохраняет массив в поле FAuthorBooks
              procedure TMBAuthor.GetAuthorBooks(const SQLQuery :  TSQLQuery);
              var
                I : Integer;
                CompositionList : String;
              begin
                   if length(FAuthorCompositions) > 0 then
                   begin
                        for I:=0 to length(FAuthorCompositions)-1 do
                        begin
                             CompositionList:=CompositionList+IntToStr(FAuthorCompositions[I]);
                             if I < length(FAuthorCompositions)-1 then CompositionList:=CompositionList+',';
                        end;
                   end;
                   SQLQuery.Close;
                   SQLQuery.SQL.Text:='SELECT book_id FROM rel_book_compositions where composition_id in=(:bComposidionsID)';
                   SQLQuery.Params.ParamByName('bCompositionsID').AsString:=CompositionList;
                   SQLQuery.Open;
                   SQLQuery.Last;
                   if SQLQuery.RecordCount > 0 then
                   begin
                       SetLength(FAuthorBooks, SQLQuery.RecordCount);
                       SQLQuery.First;
                       I:=0;
                       while not SQLQuery.EOF do
                       begin
                            FAuthorBooks[I]:=SQLQuery.FieldByName('book_id').AsLongint;
                            I:=I+1;
                            SQLQuery.Next;
                       end;
                            SQLQuery.Close;
                   end;
              end;

              procedure TMBAuthor.SetAuthorName(Name : String);
              begin
                   FAuthorName:=AuthorName;
              end;

              procedure TMBAuthor.SetAuthorSurname(Surname : String);
              begin
                   FAuthorSurname:=AuthorSurname;
              end;

              procedure TMBAuthor.UpdateAuthor(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
              begin
                   if FNewAuthor = True then
                   begin
                       SQLQuery.Close;
                       SQLQuery.SQL.Text:='inset into authors (name, surname) values (:bAuthorName, :bAuthorSurname )';
                       SQLQuery.Params.ParamByName('bAuthorName').AsString:=FAuthorName;
                       SQLQuery.Params.ParamByName('bAuthorSurname').AsString:=FAuthorSurname;
                       SQLQuery.ExecSQL;
                       SQLTransaction.Commit;
                       FNewAuthor := False;
                   end
                   else
                   begin
                       SQLQuery.Close;
                       SQLQuery.SQL.Text:='update authors set name=:bAuthorName, surname=:bAuthorSurname where id =:bAuthorID';
                       SQLQuery.Params.ParamByName('bAuthorName').AsString:=FAuthorName;
                       SQLQuery.Params.ParamByName('bAuthorSurname').AsString:=FAuthorSurname;
                       SQLQuery.Params.ParamByName('bAuthorID').AsString:=IntToStr(FAuthorID);
                       SQLQuery.ExecSQL;
                       SQLTransaction.Commit;
                   end;
                       SQLQuery.Close;
              end;

              //возвращает ID книги по индексу
              function TMBAuthor.GetAuthorBookID(Index : Integer) : Integer;
              begin
                   if Index < Length(FAuthorBooks) then
                      Result := FAuthorBooks[Index]
                   else
                      Result := -1;
              end;

              function TMBAuthor.GetAuthorCompositionID(Index : Integer) : Integer;
              begin
                   if Index < Length(FAuthorCompositions) then
                      Result := FAuthorCompositions[Index]
                   else
                       Result := -1;
              end;

procedure TMBAuthor.AddComposition(ID : Integer; const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
var
  I : Integer;
  RecordFound : Boolean = false;
begin
     //ДК, проверяем что такой записи еще нет
     for I:=0 to Length(FAuthorCompositions) do
     begin
        if ID=FAuthorCompositions[I] then
           RecordFound:=True;
     end;

     if RecordFound = False then
     begin
          SQLQuery.Close;
          SQLQuery.SQL.Text:='inset into rel_composition_authors (composition_id, author_id) values (:bAuthorID, :bCompositionID )';
          SQLQuery.Params.ParamByName('bAuthorID').AsInteger:=FAuthorID;
          SQLQuery.Params.ParamByName('bCompositionID').AsInteger:=ID;
          SQLQuery.ExecSQL;
          SQLTransaction.Commit;
          GetAuthorCompositions(SQLQuery);
     end;
end;

end.

