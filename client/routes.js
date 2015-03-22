import React from 'react';
import Router from 'react-router';
import { Route, DefaultRoute } from 'react-router';

import App from './app';
import SongContent from './views/song-content';
import Search from './views/search';
import Home from './views/home';

export default (
    <Route name="app" path="/" handler={App}>
        <DefaultRoute handler={Home} />
        <Route name="song" handler={SongContent} />
    </Route>
);
