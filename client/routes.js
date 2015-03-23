import React from 'react';
import { Route, DefaultRoute } from 'react-router';

import App from './app';

import Home from './pages/home';
import SearchResults from './pages/search-results';
import Song from './pages/song';

export default (
    <Route name="app" path="/" handler={App}>
        <DefaultRoute handler={Home} />
        <Route name="search" handler={SearchResults} />
        <Route name="song" handler={Song} />
    </Route>
);
