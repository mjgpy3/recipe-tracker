fromCategory('Recipe')
  .foreachStream()
  .when({
    $init: function (_, __) {
      return {
        recipe: null
      };
    },
    RecipeAdded: function (s, e) {
      s.recipe = e.body.recipe;
    }
  })
  .outputState();
